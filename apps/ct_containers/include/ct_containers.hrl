-export_type([ct_container_spec/0, container_id/0, port_mapping/0, port/0]).

-type ct_container_spec() ::
    #{image => binary(),
      wait_strategy => wait_strategy(),
      wait_timeout => number(),
      port_mapping => list(),
      labels => labels(),
      binds => list(),
      network => {atom(), binary()},
      alias => binary(),
      container_engine_module => module()}.
-type network_spec() ::
    #{network => atom(),
      labels => labels(),
      container_engine_module => module()}.
-type labels() :: #{binary() => binary()}.
-type container_id() :: string() | binary().
-type network_id() :: string() | binary().
-type network_name() :: atom().
-type port_mapping() :: {1..65535, tcp | udp}.
-type container_status() :: stopped | ready.
-type container_engine_cb_module() :: module().
-type wait_strategy_ctx() :: map().
-type wait_strategy() ::
    fun((container_id(), container_engine_cb_module(), wait_strategy_ctx()) ->
            {true | false, wait_strategy_ctx()}).

-define(CT_CONTAINERS_LABEL, <<"created_by.ct_containers">>).
