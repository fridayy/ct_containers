ct_containers
=====

A simplified erlang port of [testcontainers](https://www.testcontainers.org/).

> Work in progress

## Usage

`ct_containers` can either be used as a common test hook or by manually starting containers in test fixtures.

*ct_hook:*

```erlang
suite() ->
  [{timetrap, {minutes, 5}}, {ct_hooks, [{ct_containers_hook,
    [
      #{
        name => mosquitto, %% required
        image => "eclipse-mosquitto:1.6", %% required
        options => [
          {ports, [{1883, tcp}]},
          {wait_strategy, ct_containers_wait:regex(".*mosquitto version 1.6.15 running*.")},
          {timeout, 60000},
          {network, {some_network, "some_alias"}}
        ]
      }]}]}].
```

* [Raw example](https://github.com/fridayy/ct_containers/blob/master/apps/ct_containers/test/ct_containers_e2e_SUITE.erl)
* [ct hooks example](https://github.com/fridayy/ct_containers/blob/master/apps/ct_containers/test/ct_containers_e2e_cth_SUITE.erl)