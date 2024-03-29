ct_containers
=====

A simplified erlang port of [testcontainers](https://www.testcontainers.org/).

## Usage

`ct_containers` can either be used as a common test hook or by manually starting containers in test fixtures.

*ct_hook:*

```erlang
suite() ->
    [{timetrap, {minutes, 5}},
     {ct_hooks,
      [{ct_containers_hook,
        [#{lifecycle_per => suite,
           containers =>
               [#{name => httpbin,
                  image => "kennethreitz/httpbin:latest",
                  options =>
                      [{ports, [{80, tcp}]},
                       {wait_strategy,
                        ct_containers_wait:regex(".*Listening at*.")},
                       {timeout, 60000},
                       {network, {some_network, "some_alias"}}]}]}]}]}].
```

* [Raw example](https://github.com/fridayy/ct_containers/blob/master/apps/ct_containers/test/ct_containers_e2e_SUITE.erl)
* [ct hooks example](https://github.com/fridayy/ct_containers/blob/master/apps/ct_containers/test/ct_containers_e2e_cth_SUITE.erl)
