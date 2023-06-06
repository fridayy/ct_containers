%%%-------------------------------------------------------------------
%%% @author bnjm
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

suite() ->
    [
        {timetrap, {minutes, 5}},
        {ct_hooks, [
            {ct_containers_hook, [
                #{
                    lifecycle_per => suite,
                    containers =>
                        [
                            #{
                                name => httpbin,
                                image => "kennethreitz/httpbin:latest",
                                options =>
                                    [
                                        {ports, [{80, tcp}]},
                                        {wait_strategy,
                                            ct_containers_wait:regex(".*Listening at*.")},
                                        {timeout, 60000},
                                        {network, {some_network, "some_alias"}}
                                    ]
                            }
                        ]
                }
            ]}
        ]}
    ].

all() ->
    [does_connect].

does_connect(Config) ->
    Container = ct_containers:get_container(httpbin, Config),
    Host = ct_containers:host(Container, binary),
    Port = ct_containers:port(Container, {80, tcp}, binary),
    {ok, 200, _, _} = hackney:get(<<"http://", Host/binary, ":", Port/binary>>, [], <<>>, []).
