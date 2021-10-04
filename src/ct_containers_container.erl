%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2021 9:58 PM
%%%-------------------------------------------------------------------
-module(ct_containers_container).
-author("benjamin.krenn").

-behaviour(gen_statem).

-export([start_link/1, start/1, creating/3, idle/3, starting/3, exited/3, ready/3]).
-export([start_container/4, stop_container/1]).

-export([init/1, terminate/3,
  code_change/4, callback_mode/0]).

-export_type([container_status/0, container_spec/0, wait_strategy_ctx/0, wait_strategy/0, container_engine_cb_module/0, container_id/0]).


-type(container_status() :: stopped | ready).
-type(container_spec() :: #{
image => binary()
}).
-type(container_id() :: binary()).
-type(container_engine_cb_module() :: module()).
-type(wait_strategy_ctx() :: map()).
-type(wait_strategy() :: fun((container_id(), container_engine_cb_module(), wait_strategy_ctx()) -> {true | false, map()})).


-define(SERVER, ?MODULE).
-define(WATCH_POLL_INTERVAL, 100).

-record(data, {
  container_id :: container_id(),
  from :: pid(),
  container_engine_module :: container_engine_cb_module(),
  wait_strategy :: wait_strategy()
}).

start_link(ContainerEngineModule) ->
  gen_statem:start_link(?MODULE, [ContainerEngineModule], []).

start(ContainerEngineModule) ->
  gen_statem:start(?MODULE, [ContainerEngineModule], []).

-spec(start_container(pid(), container_spec(), function(), number()) -> {ok, container_status()}).
start_container(Pid, ContainerSpec, WaitStrategy, Timeout) when is_map(ContainerSpec) ->
  gen_statem:call(Pid, {start_container, ContainerSpec, WaitStrategy}, Timeout).

-spec(stop_container(pid()) -> ok).
stop_container(Pid) ->
  gen_statem:call(Pid, stop_container).

set_ready(Pid) ->
  gen_statem:cast(Pid, container_ready).

set_exited(Pid) ->
  gen_statem:cast(Pid, container_exited).

init([ContainerEngineModule]) ->
  {ok, idle, #data{container_engine_module = ContainerEngineModule},
    [
      {state_timeout, 4000, no_start_timeout}
    ]}.

callback_mode() -> state_functions.

idle({call, From}, {start_container, ContainerSpec, WaitStrategy}, Data) ->
  logger:debug(#{what => "container_idle_starting"}),
  {next_state, creating, Data#data{from = From, wait_strategy = WaitStrategy},
    [{next_event, internal, {create, ContainerSpec}}]};

idle(cast, stop_container, _Data) ->
  logger:debug(#{what => "container_idle_stopping"}),
  {stop, normal};

idle(state_timeout, no_start_timeout, _Data) ->
  {stop, normal}.

creating(internal, {create, ContainerSpec}, #data{container_engine_module = CeMod} = Data) ->
  {ok, ContainerId} = CeMod:create_container(ContainerSpec),
  {next_state, starting, Data#data{container_id = ContainerId},
    [
      {state_timeout, 4000, wait_timeout},
      {next_event, internal, start}
    ]};

creating(cast, stop_container, _Data) ->
  logger:debug(#{what => "container_creating_stopping"}),
  {stop, normal}.

starting(internal, start, #data{container_id = ContainerId, container_engine_module = CeMod, wait_strategy = WaitStrategy}) ->
  {ok, _} = CeMod:start_container(ContainerId),
  Self = self(),
  spawn_link(fun() ->
    do_watch(Self, CeMod, ContainerId, WaitStrategy)
             end),
  keep_state_and_data;

starting({call, From}, stop_container, #data{container_id = ContainerId, container_engine_module = CeMod} = Data) ->
  {ok, _} = CeMod:stop_container(ContainerId),
  {next_state, exited, Data,
    [
      {reply, From, ok},
      {next_event, internal, delete}
    ]
  };

starting(state_timeout, wait_timeout, Data) ->
  {stop, wait_strategy_timeout, Data};

starting(cast, container_ready, #data{from = From} = Data) ->
  {next_state, ready, Data, [
    {reply, From, ok}
  ]};

starting(cast, container_exited, #data{from = From} = Data) ->
  {next_state, exited, Data, [
    {reply, From, {error, container_exited}},
    {next_event, internal, delete}
  ]}.

ready({call, From}, stop_container, #data{container_id = ContainerId, container_engine_module = CeMod} = Data) ->
  {ok, _} = CeMod:stop_container(ContainerId),
  {next_state, exited, Data,
    [
      {reply, From, ok},
      {next_event, internal, delete}
    ]
  }.

exited(internal, delete, #data{container_id = ContainerId, container_engine_module = CeMod}) ->
  CeMod:delete_container(ContainerId),
  logger:debug(#{what => "container_exited_stopped"}),
  {stop, normal}.

terminate(wait_strategy_timeout, starting, #data{container_engine_module = CeMod, container_id = ContainerId}) ->
  catch {ok, _} = CeMod:stop_container(ContainerId),
  catch {ok, _} = CeMod:delete_container(ContainerId),
  ok;

terminate(normal, _State, _Data) ->
  ok;

terminate(Reason, _StateName, _State = #data{}) ->
  logger:warning(#{what => "unknown_termination", reason => Reason}),
  ok.

code_change(_OldVsn, StateName, State = #data{}, _Extra) ->
  {ok, StateName, State}.

%% private

do_watch(Pid, CeMod, ContainerId, WaitStrategy) ->
  {ok, Status} = CeMod:container_status(ContainerId),
  case Status of
    <<"exited">> ->
      set_exited(Pid);
    <<"running">> ->
      case WaitStrategy(ContainerId, CeMod, maps:new()) of
        {true, _Ctx} ->
          set_ready(Pid);
        {false, _Ctx} ->
          timer:sleep(?WATCH_POLL_INTERVAL),
          do_watch(Pid, CeMod, ContainerId, WaitStrategy)
      end;
    Else ->
      logger:warning(#{what => "watch_manager_unknown_state", state => Else}),
      {error, unknown_state}
  end.
