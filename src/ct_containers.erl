%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2021 6:21 PM
%%%-------------------------------------------------------------------
-module(ct_containers).
-author("benjamin.krenn").

-type(port_mapping() :: {1..65535, tcp | udp}).
-type(option() :: {wait_strategy, ct_containers_container:wait_strategy()}
| {timeout, pos_integer()}
| {ports, [port_mapping()]}
).
-type(options() :: [option()]).

%% API
-export([start_container/1, stop_container/1, start_container/2]).
-export_type([options/0]).

-define(DEFAULT_TIMEOUT, 5000).

-spec(start_container(string(), options()) -> {ok, pid()} | {error, container_exited}).
start_container(ImageName, Options) when is_list(ImageName) ->
  {ok, Pid} = ct_containers_container_sup:start_child(),
  ok = ct_containers_container:start_container(Pid,
    #{
      image => list_to_binary(ImageName),
      wait_strategy => proplists:get_value(wait_strategy, Options, ct_containers_wait:passthrough()),
      wait_timeout => proplists:get_value(timeout, Options, ?DEFAULT_TIMEOUT),
      port_mapping => validate_ports(proplists:get_value(ports, Options, []), [])
    }
  ),
  {ok, Pid}.

start_container(ImageName) when is_list(ImageName) ->
  start_container(ImageName, []).

stop_container(Pid) when is_pid(Pid) ->
  ct_containers_container:stop_container(Pid).

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