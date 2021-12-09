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

-record(state, {container_defs, active_containers}).

init(Id, Opts) ->
  Name = proplists:get_value(name, Opts),
  Image = proplists:get_value(image, Opts),
  CtcOpts = proplists:get_value(options, Opts),
  {ok, _Apps} = application:ensure_all_started(ct_containers),
  {ok, #state{container_defs = #{Name => {Image, CtcOpts}}}}.

pre_init_per_suite(_SuiteName, Config, #state{container_defs = ContainerDefs} = State) ->
  Containers = maps:map(fun(Name, {Image, Opts}) ->
    ct:print("Starting container ~p [~p]", [Name, Image]),
    {ok, Pid} = ct_containers:start(Image, Opts),
    Pid
                        end, ContainerDefs),
  {[{ct_containers, Containers} | Config], State#state{active_containers = Containers}}.

post_end_per_suite(_SuiteName, Config, Return, #state{active_containers = ActiveContainers} = State) ->
  Containers = maps:get(ct_containers, ActiveContainers),
  lists:foreach(fun(Name, Pid) ->
    ct:print("Stopping container ~p [~p]", [Name, Pid]),
    ct_containers:stop(Pid)
                end, Containers),
  {Config, State}.