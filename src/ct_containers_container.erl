%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2021 9:58 PM
%%%-------------------------------------------------------------------
-module(ct_containers_container).

-author("bnjm").

-behaviour(gen_statem).

-include("ct_containers.hrl").

-export([
    start_link/1,
    start/1,
    creating/3,
    idle/3,
    starting/3,
    ready/3,
    port/2,
    host/1
]).
-export([start_container/2, stop_container/1]).
-export([init/1, terminate/3, callback_mode/0]).

-define(WATCH_POLL_INTERVAL, 100).
-define(DEFAULT_NO_START_TIMEOUT, 5000).

-record(data, {
    container_id :: container_id() | undefined,
    from :: pid() | undefined,
    container_spec :: ct_container_context() | undefined,
    container_engine_module :: container_engine_cb_module()
}).

%%--------------------------------------------------------------------------
%% public API
%%--------------------------------------------------------------------------

start_link(Context) ->
    gen_statem:start_link(?MODULE, [Context], []).

start(Context) ->
    gen_statem:start(?MODULE, [Context], []).

-spec start_container(pid(), ct_container_context()) -> ok.
start_container(Pid, ContainerSpec) when is_map(ContainerSpec) ->
    logger:debug(#{what => "container_starting", spec => ContainerSpec}),
    Timeout = maps:get(wait_timeout, ContainerSpec),
    gen_statem:call(Pid, {start_container, ContainerSpec}, Timeout + 1000).

-spec stop_container(pid()) -> ok.
stop_container(Pid) ->
    gen_statem:call(Pid, stop_container).

port(Pid, PortMapping) ->
    gen_statem:call(Pid, {port, PortMapping}).

host(Pid) ->
    gen_statem:call(Pid, host).

%%--------------------------------------------------------------------------
%% private events
%%--------------------------------------------------------------------------
set_ready(Pid) ->
    gen_statem:cast(Pid, {container_ready}).

set_exited(Pid) ->
    gen_statem:cast(Pid, container_exited).

set_wait_crashed(Pid, Reason) ->
    gen_statem:cast(Pid, {wait_crashed, Reason}).

%%--------------------------------------------------------------------------
%% state machine functions
%%--------------------------------------------------------------------------

init([#{container_engine_module := ContainerEngineModule}]) ->
    {ok, idle, #data{container_engine_module = ContainerEngineModule}, [
        {state_timeout, ?DEFAULT_NO_START_TIMEOUT,
            % timeout no start call after 5s
            no_start_timeout}
    ]}.

callback_mode() ->
    state_functions.

idle({call, From}, {start_container, ContainerSpec}, Data) ->
    logger:debug(#{what => "container_idle_starting"}),
    {next_state, creating, Data#data{from = From, container_spec = ContainerSpec}, [
        {next_event, internal, create}
    ]};
idle({call, From}, stop_container, _Data) ->
    logger:debug(#{what => "container_idle_stopping"}),
    {stop_and_reply, normal, [{reply, From, ok}]};
idle(state_timeout, no_start_timeout, _Data) ->
    {stop, normal}.

creating(
    internal,
    create,
    #data{container_engine_module = CeMod, container_spec = ContainerSpec} = Data
) ->
    {ok, ContainerId} = CeMod:create_container(ContainerSpec),
    {next_state, starting, Data#data{container_id = ContainerId}, [
        {state_timeout, maps:get(wait_timeout, ContainerSpec), wait_timeout},
        {next_event, internal, start}
    ]};
creating(cast, stop_container, _Data) ->
    logger:debug(#{what => "container_creating_stopping"}),
    {stop, normal}.

starting(
    internal,
    start,
    #data{
        container_id = ContainerId,
        container_engine_module = CeMod,
        container_spec = ContainerSpec
    }
) ->
    {ok, _} = CeMod:start_container(ContainerId),
    #{wait_strategy := WaitStrategy, wait_timeout := Timeout} = ContainerSpec,
    Self = self(),
    spawn_link(fun() ->
        case catch do_watch(Self, CeMod, ContainerId, WaitStrategy, maps:new()) of
            ok ->
                ok;
            Reason ->
                set_wait_crashed(Self, Reason)
        end
    end),
    {keep_state_and_data, [{state_timeout, Timeout, wait_timeout}]};
starting(cast, {wait_crashed, Reason}, #data{from = From}) ->
    {stop_and_reply, wait_strategy_timeout, [{reply, From, {error, wait_crashed, Reason}}]};
starting(
    {call, From},
    stop_container,
    #data{
        container_id = ContainerId, container_engine_module = CeMod, container_spec = ContainerSpec
    }
) ->
    do_stop(CeMod, ContainerId, ContainerSpec),
    {stop_any_reply, normal, [{reply, From, ok}]};
starting(state_timeout, wait_timeout, #data{from = From}) ->
    {stop_and_reply, wait_strategy_timeout, [{reply, From, {error, wait_timeout}}]};
starting(cast, {container_ready}, #data{from = From} = Data) ->
    {next_state, ready, Data, [{reply, From, ok}]};
starting(cast, container_exited, #data{from = From}) ->
    {stop_and_reply, normal, [
        {reply, From, {error, container_exited}}
    ]};
starting(cast, stop_container, _Data) ->
    logger:info("stopping 'starting' container"),
    {stop, normal}.

ready(
    {call, From},
    stop_container,
    #data{
        container_id = ContainerId, container_engine_module = CeMod, container_spec = ContainerSpec
    }
) ->
    do_stop(CeMod, ContainerId, ContainerSpec),
    {stop_and_reply, normal, [{reply, From, ok}]};
ready(
    {call, From},
    {port, PortMapping},
    #data{container_id = ContainerId, container_engine_module = CeMod}
) ->
    R = CeMod:port(ContainerId, PortMapping),
    {keep_state_and_data, [{reply, From, R}]};
ready(
    {call, From},
    host,
    #data{container_id = ContainerId, container_engine_module = CeMod}
) ->
    {ok, IpAddr} = CeMod:host(ContainerId),
    %% could be either an ip address or a hostname represented as a binary
    Host =
        case inet:parse_address(binary_to_list(IpAddr)) of
            {ok, Addr} ->
                Addr;
            {error, einval} ->
                logger:debug("could not parse host address '~p'", [IpAddr]),
                binary_to_list(IpAddr)
        end,
    {keep_state_and_data, [{reply, From, {ok, Host}}]}.

terminate(
    Reason,
    _,
    #data{container_engine_module = CeMod, container_id = ContainerId, container_spec = Spec}
) ->
    logger:warning(#{what => "container_termination", why => Reason}),
    do_stop(CeMod, ContainerId, Spec),
    ok.

%% private

do_watch(Pid, CeMod, ContainerId, WaitStrategy, Context) ->
    {ok, Status} = CeMod:status(ContainerId),
    case Status of
        <<"exited">> ->
            set_exited(Pid);
        <<"running">> ->
            case WaitStrategy(ContainerId, CeMod, Context) of
                {true, _NewContext} ->
                    set_ready(Pid);
                {false, NewContext} ->
                    timer:sleep(?WATCH_POLL_INTERVAL),
                    do_watch(Pid, CeMod, ContainerId, WaitStrategy, NewContext)
            end;
        Else ->
            logger:warning(#{what => "watch_manager_unknown_state", state => Else}),
            {error, unknown_state}
    end.

do_stop(CeMod, ContainerId, #{
    network := {Name, _Alias}
}) when is_binary(ContainerId) ->
    NetworkId = erlang:atom_to_binary(Name),
    catch {ok, _} = CeMod:detach_container(NetworkId, ContainerId),
    catch {ok, _} = CeMod:stop_container(ContainerId),
    catch {ok, _} = CeMod:delete_container(ContainerId),
    ok;
do_stop(CeMod, ContainerId, _) when is_binary(ContainerId) ->
    catch {ok, _} = CeMod:stop_container(ContainerId),
    catch {ok, _} = CeMod:delete_container(ContainerId),
    ok.
