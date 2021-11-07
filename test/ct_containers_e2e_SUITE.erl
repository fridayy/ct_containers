%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Nov 2021 6:50 PM
%%%-------------------------------------------------------------------
-module(ct_containers_e2e_SUITE).
-author("bnjm").

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0,
  init_per_suite/1, end_per_suite/1, does_connect/1]).

suite() ->
  [{timetrap, {minutes, 1}}].

init_per_suite(Config) ->
  {ok, _Apps} = application:ensure_all_started(ct_containers),
  {ok, Pid} = ct_containers:start("eclipse-mosquitto:1.6",
    [
      {ports, [{1883, tcp}]},
      {wait_strategy, ct_containers_wait:regex(".*mosquitto version 1.6.15 running*.")}
    ]),
  {ok, MappedPort} = ct_containers:port(Pid, {1883, tcp}),
  [{ct_containers_pid, Pid}, {ct_container_port, MappedPort} | Config].

end_per_suite(Config) ->
  Pid = proplists:get_value(ct_containers_pid, Config),
  ct_containers:stop(Pid).

all() ->
  [does_connect].

does_connect(Config) ->
  {ok, ClientPid} = emqtt:start_link([
    {host, "localhost"},
    {port, proplists:get_value(ct_container_port, Config)}
  ]),
  {ok, _} = emqtt:connect(ClientPid),
  ok.