%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dec 2021 8:37 PM
%%%-------------------------------------------------------------------
-module(ct_containers_hook).
-author("benjamin.krenn").

%% API
-export([pre_init_per_suite/3, post_end_per_suite/4, init/2]).

-type ct_hook_container_spec() :: #{
  name => atom(),
  image => string(),
  options => list()
}.

-record(state, {container_defs, active_containers}).

init(_Id, ContainerDefinitions) ->
  {ok, _Apps} = application:ensure_all_started(ct_containers),
  {ok, #state{container_defs = ContainerDefinitions}}.

pre_init_per_suite(_SuiteName, Config, #state{container_defs = ContainerDefs} = State) ->
  Containers = lists:map(fun(M) ->
                          #{name := Name, image := Image, options := Opts} = M,
                          ct:print("Starting container [~p]", [Image]),
                          {ok, Pid} = ct_containers:start(Image, Opts),
                          {Name, Pid}
                        end, ContainerDefs),
  ContainersByName = maps:from_list(Containers),
  {[{ct_containers, ContainersByName} | Config], State#state{active_containers = ContainersByName}}.

post_end_per_suite(_SuiteName, Config, Return, #state{active_containers = ActiveContainers} = State) ->
  maps:foreach(fun(Name, Pid) ->
    ct:print("Stopping container ~p [~p]", [Name, Pid]),
    ct_containers:stop(Pid)
                end, ActiveContainers),
    ct_containers:delete_networks(),
  {Config, State}.