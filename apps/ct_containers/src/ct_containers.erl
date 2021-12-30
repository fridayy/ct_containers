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

-type(option() :: {wait_strategy, wait_strategy()}
| {timeout, pos_integer()}
| {ports, [port_mapping()]}
| {ryuk, boolean()}
| {volumes, list()}
).
-type(options() :: [option()]).

%% API
-export([start/1, stop/1, start/2, port/2, host/1]).
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
%% @end
-spec(start(string(), options()) -> {ok, pid()} | {error, container_exited}).
start(ImageName, Options) when is_list(ImageName) ->
  ContainerRuntimeModule = proplists:get_value(runtime, Options, ct_containers_docker),
  {ok, Pid} = ct_containers_container_sup:start_child(ContainerRuntimeModule),
  ok = ct_containers_container:start_container(Pid,
    #{
      image => list_to_binary(ImageName),
      wait_strategy => proplists:get_value(wait_strategy, Options, ct_containers_wait:passthrough()),
      wait_timeout => proplists:get_value(timeout, Options, ?DEFAULT_TIMEOUT),
      port_mapping => validate_ports(proplists:get_value(ports, Options, []), []),
      labels => #{?CT_CONTAINERS_LABEL => <<"true">>},
      binds => proplists:get_value(volumes, Options, [])
    }
  ),
  {ok, Pid}.

start(ImageName) when is_list(ImageName) ->
  start(ImageName, []).

-spec(stop(pid()) -> ok).
stop(Pid) when is_pid(Pid) ->
  ct_containers_container:stop_container(Pid).

-spec(port(pid(), port_mapping()) -> {ok, integer()} | {error, no_port}).
port(Pid, PortMapping) ->
  ct_containers_container:port(Pid, PortMapping).

-spec(host(pid()) -> string() | inet:ip4_address()).
host(Pid) ->
  {ok, Host} = ct_containers_container:host(Pid),
  Host.

%% private

%%% @doc
%%% Throws if given an invalid port - otherwise returns the ports
%%% @end
-spec(validate_ports([port_mapping()], [port_mapping()]) -> true | false).
validate_ports([H | T], ValidPorts) ->
  IsValid = validate_port(H),
  if
    IsValid =:= false -> throw(invalid_port);
    true -> validate_ports(T, [H | ValidPorts])
  end;

validate_ports([], ValidPorts) -> ValidPorts.

validate_port({Port, tcp}) when is_number(Port), Port > 1, Port < 65535 ->
  true;

validate_port({Port, udp}) when is_number(Port), Port > 1, Port < 65535 ->
  true;

validate_port(_Else) -> false.