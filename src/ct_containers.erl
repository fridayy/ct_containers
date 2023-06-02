%%%-------------------------------------------------------------------
%% @author bnjm
%% @copyright (C) 2021, leftshift.one software gmbh
%% @doc
%%
%% @end
%% Created : 03. Oct 2021 6:21 PM
%%%-------------------------------------------------------------------
-module(ct_containers).

-author("bnjm").

-export([
    start/1,
    stop/1,
    start/2,
    port/2,
    port/3,
    host/1,
    host/2,
    delete_networks/0
]).

-include("ct_containers.hrl").

-type option() ::
    {wait_strategy, wait_strategy()}
    | {timeout, pos_integer()}
    | {ports, [port_mapping()]}
    | {volumes, list()}
    | {runtime, module()}
    | {network, {atom(), string()}}
    | {env, #{binary() => binary()}}.
-type options() :: [option()].

-include_lib("kernel/include/logger.hrl").

%% API
-export_type([options/0]).

-define(DEFAULT_TIMEOUT, 5000).

%% @doc Starts the given image name using the configured container runtime (docker by default).
%% The following options are available:
%% <ul>
%% <li>`{runtime, RuntimeModule::module()}': The container runtime module to be used</li>
%% <li>`{wait_strategy, WaitStrategyFunction::function()}': The applied wait strategy for the container - default: passthrough
%%  meaning the container will be considered as 'ready' as soon as it switched to 'running' state.
%% </li>
%% <li>`{timeout, Timeout::timeout()}': The timespan for a wait strategy to finish before being considered failed (default: 5000)</li>
%% <li>`{ports, [port_mapping()]}': The open ports for this container
%% <li>`{network, {NetworkName::atom(), Alias::string()}}': The network this container will be attached to and the network alias for inter container networking</li>
%% <li>`{}': </li>
%% <li>`{}': </li>
%% </ul>
%% @end
-spec start(string(), options()) -> {ok, pid()} | {error, container_exited}.
start(ImageName, Options) when is_list(ImageName) ->
    Context = build_context(ImageName, Options),
    {ok, ContainerPid} = ct_containers_container_sup:start_child(Context),
    case ct_containers_network_sup:start_child(Context) of
        {ok, NetworkPid} ->
            {ok, _NetworkId} = ct_containers_network:create(NetworkPid);
        _ ->
            ok
    end,
    ok = ct_containers_container:start_container(ContainerPid, Context),
    {ok, ContainerPid}.

start(ImageName) when is_list(ImageName) ->
    start(ImageName, []).

-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) ->
    ct_containers_container:stop_container(Pid).

delete_networks() ->
    Networks = supervisor:which_children(ct_containers_network_sup),
    lists:foreach(
        fun({_I, Pid, _T, _M}) ->
            ct_containers_network:delete(Pid),
            logger:info("deleted network [~p]", [Pid])
        end,
        Networks
    ),
    ok.

-spec port(pid(), port_mapping()) -> integer().
port(Pid, PortMapping) ->
    {ok, Port} = ct_containers_container:port(Pid, PortMapping),
    Port.

port(Pid, PortMapping, binary) ->
    erlang:integer_to_binary(port(Pid, PortMapping)).

-spec host(pid()) -> string() | inet:ip4_address().
host(Pid) ->
    {ok, Host} = ct_containers_container:host(Pid),
    Host.

host(Pid, binary) ->
    Host = host(Pid),
    NewHost =
        case inet:is_ip_address(Host) of
            true ->
                inet:ntoa(Host);
            false ->
                Host
        end,
    erlang:list_to_binary(NewHost).

%% private parts

%%% @doc
%%% Throws if given an invalid port - otherwise returns the ports
%%% @end
-spec validate_ports([port_mapping()], [port_mapping()]) ->
    {ok, [port_mapping]} | {error, invalid_port}.
validate_ports([], ValidPorts) ->
    {ok, ValidPorts};
validate_ports([H | T], ValidPorts) ->
    IsValid = validate_port(H),
    if
        IsValid =:= false ->
            {error, invalid_port};
        true ->
            validate_ports(T, [H | ValidPorts])
    end.

validate_port({Port, tcp}) when is_number(Port), Port > 1, Port < 65535 ->
    true;
validate_port({Port, udp}) when is_number(Port), Port > 1, Port < 65535 ->
    true;
validate_port(_Else) ->
    false.

-spec build_context(string(), options()) -> ct_container_context().
build_context(ImageName, Options) ->
    %% options
    ContainerEngineModule = proplists:get_value(runtime, Options, ct_containers_docker),
    WaitStrategy = proplists:get_value(wait_strategy, Options, ct_containers_wait:passthrough()),
    WaitTimeout = proplists:get_value(timeout, Options, ?DEFAULT_TIMEOUT),
    Volumes = proplists:get_value(volumes, Options, []),
    Network = proplists:get_value(
        network, Options, undefined
    ),
    {ok, Ports} = validate_ports(proplists:get_value(ports, Options, []), []),
    %% labels TODO: evaluate option for addin labels
    Labels = #{?CT_CONTAINERS_LABEL => <<"true">>},
    Env = proplists:get_value(env, Options, #{}),

    #{
        image => list_to_binary(ImageName),
        wait_strategy => WaitStrategy,
        wait_timeout => WaitTimeout,
        port_mapping => Ports,
        labels => Labels,
        binds => Volumes,
        network => Network,
        env => Env,
        container_engine_module => ContainerEngineModule
    }.
