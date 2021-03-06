%%%-------------------------------------------------------------------
%% @author benjamin.krenn
%% @copyright (C) 2021, leftshift.one software gmbh
%% @doc
%%
%% @end
%% Created : 03. Oct 2021 6:21 PM
%%%-------------------------------------------------------------------
-module(ct_containers).

-author("benjamin.krenn").

-include("ct_containers.hrl").

-type option() ::
        {wait_strategy, wait_strategy()} |
        {timeout, pos_integer()} |
        {ports, [port_mapping()]} |
        {ryuk, boolean()} |
        {volumes, list()} |
        {network, {atom(), string()}}.
-type options() :: [option()].

%% API
-export([start/1, stop/1, start/2, port/2, host/1, delete_networks/0]).

-export_type([options/0]).

-define(DEFAULT_TIMEOUT, 5000).

%% @doc Starts the given image name using the configured container runtime (docker by default).
%% The following options are available:
%% <ul>
%% <li>`{runtime, Module}': The container runtime module to be used</li>
%% <li>`{wait_strategy, Fun}': The applied wait strategy for the container - default: passthrough
%%  meaning the container will be considered as 'ready' as soon as it switched to 'running' state.
%% </li>
%% <li>`{wait_timeout, Number}': </li>
%% </ul>
%% @end
-spec start(string(), options()) -> {ok, pid()} | {error, container_exited}.
start(ImageName, Options) when is_list(ImageName) ->
    Context = build_context(ImageName, Options),
    {ok, ContainerPid} = ct_containers_container_sup:start_child(Context),
    %% ignore the return value - if there is already a network created with the given name
    %% there is nothing to do anyways
    {ok, NetworkPid} = ct_containers_network_sup:start_child(Context),
    {ok, _NetworkId} = ct_containers_network:create(NetworkPid),
    ok = ct_containers_container:start_container(ContainerPid, Context), %% TODO: refactor to arity 1 as context is already passed to/via the supervisor
    {ok, ContainerPid}.

start(ImageName) when is_list(ImageName) ->
    start(ImageName, []).

-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) ->
    ct_containers_container:stop_container(Pid).

delete_networks() ->
    Networks = supervisor:which_children(ct_containers_network_sup),
    lists:foreach(fun({_I, Pid, _T, _M}) ->
                          ct_containers_network:delete(Pid),
                          logger:info("deleted network [~p]", [Pid])
                  end,
                  Networks),
    ok.

-spec port(pid(), port_mapping()) -> {ok, integer()} | {error, no_port}.
port(Pid, PortMapping) ->
    ct_containers_container:port(Pid, PortMapping).

-spec host(pid()) -> string() | inet:ip4_address().
host(Pid) ->
    {ok, Host} = ct_containers_container:host(Pid),
    Host.

%% private

%%% @doc
%%% Throws if given an invalid port - otherwise returns the ports
%%% @end
-spec validate_ports([port_mapping()], [port_mapping()]) ->
          {ok, [port_mapping]} | {error, invalid_port}.
validate_ports([], ValidPorts) ->
    {ok, ValidPorts};
validate_ports([H | T], ValidPorts) ->
    IsValid = validate_port(H),
    if IsValid =:= false ->
            {error, invalid_port};
       true ->
            validate_ports(T, [H | ValidPorts])
    end.

validate_port({Port, tcp}) when is_number(Port), Port > 1, Port < 65535 ->
    true;
validate_port({Port, udp}) when is_number(Port), Port > 1, Port < 65535 ->
    true;
validate_port(_Else) ->
    false.

-spec build_context(string(), options()) -> ct_container_context().
build_context(ImageName, Options) ->
    Labels = #{?CT_CONTAINERS_LABEL => <<"true">>},
    ContainerEngineModule = proplists:get_value(runtime, Options, ct_containers_docker),
    {Network, Alias} = proplists:get_value(network, Options, {ct_default_network, random_network_alias()}),
    {ok, Ports} = validate_ports(proplists:get_value(ports, Options, []), []),
    #{image => list_to_binary(ImageName),
      wait_strategy =>
          proplists:get_value(wait_strategy, Options, ct_containers_wait:passthrough()),
      wait_timeout => proplists:get_value(timeout, Options, ?DEFAULT_TIMEOUT),
      port_mapping => Ports,
      labels => Labels,
      binds => proplists:get_value(volumes, Options, []),
      network => {Network, list_to_binary(Alias)},
      container_engine_module => ContainerEngineModule}.

-spec random_network_alias() -> string().
random_network_alias() ->
    Hash = erlang:phash2(erlang:make_ref()),
    logger:debug("no network alias set using random alias '~p'", [Hash]),
    erlang:integer_to_list(Hash).