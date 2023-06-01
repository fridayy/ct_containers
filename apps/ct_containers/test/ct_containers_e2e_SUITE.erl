%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one
%%% @doc
%%% This e2e test, beside being a test, showcases how ct_containers
%%% can be used in common tests
%%% @end
%%% Created : 07. Nov 2021 6:50 PM
%%%-------------------------------------------------------------------
-module(ct_containers_e2e_SUITE).
-compile([export_all]).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

suite() ->
    [{timetrap, {minutes, 5}}].

init_per_suite(Config) ->
    {ok, _Apps} = application:ensure_all_started(ct_containers),
    {ok, Pid} =
        ct_containers:start("eclipse-mosquitto:1.6",
                            [{ports, [{1883, tcp}]},
                             {wait_strategy,
                              ct_containers_wait:regex(".*mosquitto version 1.6.15 running*.")},
                             {timeout, 60000},
                             {network, {some_network, "some_alias"}}]),
    ContainerHost = ct_containers:host(Pid),
    {ok, MappedPort} = ct_containers:port(Pid, {1883, tcp}),
    [{ct_containers_pid, Pid},
     {ct_container_port, MappedPort},
     {ct_container_host, ContainerHost}
     | Config].

end_per_suite(Config) ->
    Pid = proplists:get_value(ct_containers_pid, Config),
    ct_containers:stop(Pid),
    ct_containers:delete_networks(),
    application:stop(ct_containers),
    Config.

all() ->
    [does_connect].

does_connect(Config) ->
    Host = proplists:get_value(ct_container_host, Config),
    Port = proplists:get_value(ct_container_port, Config),
    {ok, ClientPid} = emqtt:start_link([{host, Host}, {port, Port}]),
    {ok, _} = emqtt:connect(ClientPid),
    ok = emqtt:publish(ClientPid, <<"hello">>, #{}, <<"Hello World!">>, [{qos, 0}]),
    ok = emqtt:disconnect(ClientPid),
    ok = emqtt:stop(ClientPid),
    ?assertNot(erlang:is_process_alive(ClientPid)).
