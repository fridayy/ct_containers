-module(ct_containers_container_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MOCK_ENGINE_NAME, test_container_engine).

%% running container tests
running_container_test_() ->
    {foreach,
     fun running_container_setup/0,
     fun teardown/1,
     [fun happy_path/1,
      fun delaying_wait_strategy/1,
      fun times_out_if_wait_strategy_always_false/1,
      fun times_out_if_wait_strategy_blocks_indefinitely/1,
      fun throwing_wait_strategy_does_not_compromise_system/1]}.

running_container_setup() ->
    meck:new(?MOCK_ENGINE_NAME, [non_strict]),
    meck:expect(?MOCK_ENGINE_NAME, create_container, fun(_) -> {ok, <<"SomeId">>} end),
    meck:expect(?MOCK_ENGINE_NAME, start_container, [<<"SomeId">>], {ok, <<"SomeId">>}),
    meck:expect(?MOCK_ENGINE_NAME, status, ['_'], {ok, <<"running">>}),
    meck:expect(?MOCK_ENGINE_NAME, inspect, [<<"SomeId">>], {ok, #{}}),
    meck:expect(?MOCK_ENGINE_NAME, delete_container, ['_'], {ok, <<"SomeId">>}),
    meck:expect(?MOCK_ENGINE_NAME, stop_container, ['_'], {ok, <<"SomeId">>}),
    {ok, Pid} = ct_containers_container:start(?MOCK_ENGINE_NAME),
    Pid.

teardown(Pid) ->
    meck:unload(?MOCK_ENGINE_NAME),
    case erlang:process_info(Pid) of
        undefined ->
            ok;
        _ ->
            gen_statem:stop(Pid)
    end.

happy_path(Pid) ->
    R = ct_containers_container:start_container(Pid,
                                                #{wait_strategy => ct_containers_wait:passthrough(),
                                                  wait_timeout => 100}),
    [?_assertEqual(ok, R)].

delaying_wait_strategy(Pid) ->
    WaitStrategy =
        fun(_Id, _Cm, Ctx) ->
           Count = maps:get(count, Ctx, 0),
           if Count > 3 ->
                  {true, Ctx};
              true ->
                  {false, maps:put(count, Count + 1, Ctx)}
           end
        end,
    R = ct_containers_container:start_container(Pid,
                                                #{wait_strategy => WaitStrategy,
                                                  wait_timeout => 1500}),
    [?_assertEqual(ok, R)].

times_out_if_wait_strategy_always_false(Pid) ->
    WaitStrategy = fun(_Id, _Cm, Ctx) -> {false, Ctx} end,
    R = ct_containers_container:start_container(Pid,
                                                #{wait_strategy => WaitStrategy,
                                                  wait_timeout => 1500}),
    [?_assertEqual({error, wait_timeout}, R)].

times_out_if_wait_strategy_blocks_indefinitely(Pid) ->
    WaitStrategy = fun(_Id, _Cm, _Ctx) -> timer:sleep(9999999) end,
    R = ct_containers_container:start_container(Pid,
                                                #{wait_strategy => WaitStrategy,
                                                  wait_timeout => 1500}),
    [?_assertEqual({error, wait_timeout}, R),
     ?_assertEqual(false, erlang:is_process_alive(Pid))].

throwing_wait_strategy_does_not_compromise_system(Pid) ->
    WaitStrategy = fun(_Id, _Cm, _Ctx) -> throw(goodbye_cruel_world) end,
    R = ct_containers_container:start_container(Pid,
                                                #{wait_strategy => WaitStrategy,
                                                  wait_timeout => 1500}),
    [?_assertEqual({error, wait_crashed, goodbye_cruel_world}, R),
     ?_assertEqual(false, erlang:is_process_alive(Pid))].

%% exiting container tests
exiting_container_test_() ->
    {foreach,
     fun exiting_container_setup/0,
     fun teardown/1,
     [fun immediately_returns_if_container_exited/1]}.

exiting_container_setup() ->
    meck:new(?MOCK_ENGINE_NAME, [non_strict]),
    meck:expect(?MOCK_ENGINE_NAME, create_container, fun(_) -> {ok, <<"SomeId">>} end),
    meck:expect(?MOCK_ENGINE_NAME, start_container, [<<"SomeId">>], {ok, <<"SomeId">>}),
    meck:expect(?MOCK_ENGINE_NAME, status, ['_'], {ok, <<"exited">>}),
    meck:expect(?MOCK_ENGINE_NAME, inspect, [<<"SomeId">>], {ok, #{}}),
    meck:expect(?MOCK_ENGINE_NAME, delete_container, ['_'], {ok, <<"SomeId">>}),
    meck:expect(?MOCK_ENGINE_NAME, stop_container, ['_'], {ok, <<"SomeId">>}),
    {ok, Pid} = ct_containers_container:start(?MOCK_ENGINE_NAME),
    Pid.

immediately_returns_if_container_exited(Pid) ->
    R = ct_containers_container:start_container(Pid,
                                                #{wait_strategy => ct_containers_wait:passthrough(),
                                                  wait_timeout => 500}),
    [?_assertEqual({error, container_exited}, R),
     ?_assertEqual(false, erlang:is_process_alive(Pid))].
