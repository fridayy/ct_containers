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
-compile([export_all]).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

suite() ->
    [{timetrap, {minutes, 5}},
     {ct_hooks,
      [{ct_containers_hook,
        [#{lifecycle_per => suite,
           containers =>
               [#{name => mosquitto,
                  image => "eclipse-mosquitto:1.6",
                  options =>
                      [{ports, [{1883, tcp}]},
                       {wait_strategy,
                        ct_containers_wait:regex(".*mosquitto version 1.6.15 running*.")},
                       {timeout, 60000},
                       {network, {some_network, "some_alias"}}]}]}]}]}].

all() ->
    [does_connect].

does_connect(Config) ->
    Containers = proplists:get_value(ct_containers, Config),
    MosquittoContainer = maps:get(mosquitto, Containers),
    Host = ct_containers:host(MosquittoContainer),
    {ok, Port} = ct_containers:port(MosquittoContainer, {1883, tcp}),
    {ok, ClientPid} = emqtt:start_link([{host, Host}, {port, Port}]),
    {ok, _} = emqtt:connect(ClientPid),
    ok = emqtt:publish(ClientPid, <<"hello">>, #{}, <<"Hello World!">>, [{qos, 0}]),
    ok = emqtt:disconnect(ClientPid),
    ok = emqtt:stop(ClientPid),
    ?assertNot(erlang:is_process_alive(ClientPid)).
