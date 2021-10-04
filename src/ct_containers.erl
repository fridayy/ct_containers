%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2021 6:21 PM
%%%-------------------------------------------------------------------
-module(ct_containers).
-author("benjamin.krenn").

-type(option() :: {wait_strategy, ct_containers_container:wait_strategy()} | {timeout, pos_integer()}
).
-type(options() :: [option()]).

%% API
-export([start_container/1, stop_container/1, start_container/2]).
-export_type([options/0]).

-define(DEFAULT_TIMEOUT, 5000).

-spec(start_container(string(), options()) -> {ok, pid()} | {error, container_exited}).
start_container(ImageName, Options) when is_list(ImageName) ->
  {ok, Pid} = ct_containers_container_sup:start_child(),
  ok = ct_containers_container:start_container(Pid,
    #{image => list_to_binary(ImageName)},
    proplists:get_value(wait_strategy, Options, ct_containers_wait:passthrough()),
    proplists:get_value(timeout, Options, ?DEFAULT_TIMEOUT)
  ),
  {ok, Pid}.

start_container(ImageName) when is_list(ImageName) ->
  start_container(ImageName, []).

stop_container(Pid) when is_pid(Pid) ->
  ct_containers_container:stop_container(Pid).

