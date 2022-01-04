%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one
%%% @doc
%%% This e2e test, beside being a test, showcases how ct_containers
%%% can be used in common tests
%%% @end
%%% Created : 07. Nov 2021 6:50 PM
%%%-------------------------------------------------------------------
-module(ct_containers_e2e_cth_SUITE).
-author("benjamin.krenn").

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, does_connect/1]).

suite() ->
  [{timetrap, {minutes, 5}}, {ct_hooks, [{ct_containers_hook,
    [
      #{
        name => mosquitto,
        image => "eclipse-mosquitto:1.6",
        options => [
          {ports, [{1883, tcp}]},
          {wait_strategy, ct_containers_wait:regex(".*mosquitto version 1.6.15 running*.")},
          {timeout, 60000},
          {network, {some_network, "some_alias"}}
        ]
      }]}]}].

all() ->
  [does_connect].

does_connect(Config) ->
  Containers = proplists:get_value(ct_containers, Config),
  MosquittoContainer = maps:get(mosquitto, Containers),
  Host = ct_containers:host(MosquittoContainer),
  {ok, Port} = ct_containers:port(MosquittoContainer, {1883, tcp}),
  ct:print("Connecting to ~p:~p", [Host, Port]),
  {ok, ClientPid} = emqtt:start_link([
    {host, Host},
    {port, Port}
  ]),
  {ok, _} = emqtt:connect(ClientPid),
  ok.
