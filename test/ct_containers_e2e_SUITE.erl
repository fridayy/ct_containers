%%%-------------------------------------------------------------------
%%% @author bnjm
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
    {ok, _} = application:ensure_all_started(hackney),
    {ok, Pid} =
        ct_containers:start(
            "kennethreitz/httpbin:latest",
            [
                {ports, [{80, tcp}]},
                {wait_strategy, ct_containers_wait:regex(".*Listening at*.")},
                {timeout, 60000},
                {network, {some_network, "some_alias"}}
            ]
        ),
    ContainerHost = ct_containers:host(Pid),
    {ok, MappedPort} = ct_containers:port(Pid, {80, tcp}),
    [
        {ct_containers_pid, Pid},
        {ct_container_port, erlang:integer_to_binary(MappedPort)},
        {ct_container_host, erlang:list_to_binary(ContainerHost)}
        | Config
    ].

end_per_suite(Config) ->
    Pid = proplists:get_value(ct_containers_pid, Config),
    ct_containers:stop(Pid),
    ct_containers:delete_networks(),
    Config.

all() ->
    [does_connect].

does_connect(Config) ->
    Host = proplists:get_value(ct_container_host, Config),
    Port = proplists:get_value(ct_container_port, Config),
    {ok, 200, _, _} = hackney:get(<<"http://", Host/binary, ":", Port/binary>>, [], <<>>, []).
