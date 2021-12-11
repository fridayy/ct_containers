%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ct_containers_docker).

-export([create_container/1, start_container/1, stop_container/1, delete_container/1, container_logs/1, inspect/1, status/1, host/1, port/2, pull_image/1]).

-type(container_info() :: map()).

-define(SERVER, ?MODULE).
-define(DOCKER_SOCKET, "/var/run/docker.sock").

-spec(create_container(ct_containers_container:ct_container_spec()) -> {ok, binary()}).
create_container(ContainerSpec) ->
  Url = docker_url(<<"/containers/create">>),
  #{image := Image, port_mapping := PortMapping, labels := Labels} = ContainerSpec,
  DockerContainerSpec = #{
    <<"Image">> => Image,
    <<"Labels">> => Labels,
    <<"ExposedPorts">> => map_ports(PortMapping, #{}),
    <<"HostConfig">> => #{
      <<"PortBindings">> => map_ports(PortMapping, [#{<<"HostPort">> => <<"">>}])
    }
  },
  case ct_containers_http:post(Url, DockerContainerSpec) of
    {201, #{<<"Id">> := ContainerId}} ->
      logger:info(#{what => "docker_engine_container_created"}),
      {ok, ContainerId};
    {404, _} ->
      logger:info(#{what => "docker_engine_container_pull"}),
      pull_image(Image),
      create_container(ContainerSpec)
  end.

pull_image(Image) when is_binary(Image) ->
  Url = docker_url(<<"/images/create?fromImage=", Image/binary>>),
  {200, _} = ct_containers_http:post(Url, #{}),
  ok.

-spec(start_container(binary()) -> {ok, binary()}).
start_container(ContainerId) ->
  Url = docker_url(<<"/containers/", ContainerId/binary, "/start">>),
  {204, _} = ct_containers_http:post(Url, #{}),
  logger:info(#{what => "docker_engine_container_started"}),
  {ok, ContainerId}.

-spec(stop_container(binary()) -> {ok, binary()}).
stop_container(ContainerId) ->
  Url = docker_url(<<"/containers/", ContainerId/binary, "/stop">>),
  case ct_containers_http:post(Url, #{}) of
    {204, _} ->
      logger:info(#{what => "docker_engine_container_stopped"}),
      {ok, ContainerId};
    {304, _} ->
      logger:warning(#{what => "docker_engine_container_already_stopped", action => "skipping"}),
      {ok, ContainerId}
  end.

-spec(delete_container(binary()) -> {ok, binary()}).
delete_container(ContainerId) ->
  Url = docker_url(<<"/containers/", ContainerId/binary>>),
  {204, _} = ct_containers_http:delete(Url),
  logger:info(#{what => "docker_engine_container_deleted"}),
  {ok, ContainerId}.

-spec(inspect(binary()) -> {ok, container_info()}).
inspect(ContainerId) ->
  Url = docker_url(<<"/containers/", ContainerId/binary, "/json">>),
  {200, ContainerInfo} = ct_containers_http:get(Url),
  logger:info(#{what => "docker_engine_container_inspected"}),
  {ok, ContainerInfo}.

container_logs(ContainerId) ->
  Url = docker_url(<<"/containers/", ContainerId/binary, "/logs?stdout=true&stderr=true">>),
  {ok, Logs} = ct_containers_http:get_plain(Url),
  logger:info(#{what => "docker_engine_container_logs_read"}),
  {ok, Logs}.

%%% @doc
%%% Extracts the status information from a ContainerInfo acquired by inspect/1
%%% @end
-spec(status(container_info()) -> {ok, binary()}).
status(ContainerInfo) ->
  #{<<"State">> := #{<<"Status">> := Status}} = ContainerInfo,
  {ok, Status}.

%%% @doc
%%% Reads the host address from a ContainerInfo acquired by inspect/1
-spec(host(container_info()) -> {ok, binary()}).
host(ContainerInfo) ->
  case running_in_container() of
    false -> {ok, <<"localhost">>};
    true ->
      #{<<"NetworkSettings">> := #{<<"Gateway">> := IpAddress}} = ContainerInfo,
      {ok, IpAddress}
  end.

-spec(port(ct_containers:port_mapping(), container_info()) -> {ok, 1..65565}).
port({Port, tcp}, ContainerInfo) ->
  #{<<"NetworkSettings">> := #{<<"Ports">> := PortMappings}} = ContainerInfo,
  PortBinary = erlang:integer_to_binary(Port),
  TcpBinary = erlang:atom_to_binary(tcp),
  PortMappingsKey = <<PortBinary/binary, "/", TcpBinary/binary>>,
  case maps:is_key(PortMappingsKey, PortMappings) of
    false -> {error, no_port};
    true ->
      [#{<<"HostPort">> := MappedPort} | _] = maps:get(PortMappingsKey, PortMappings),
      if
        MappedPort =:= null -> {error, no_port};
        true -> {ok, erlang:binary_to_integer(MappedPort)}
      end
  end.


%% private
running_in_container() ->
  filelib:is_file("/.dockerenv").

docker_url(Path) ->
  UrlEncodedSocketLocation = hackney_url:urlencode(?DOCKER_SOCKET),
  <<"http+unix://", UrlEncodedSocketLocation/binary, Path/binary>>.

%%% @doc
%%% maps ports from the ct_containers format to the docker format
%%% @end
-spec(map_ports([{number(), tcp | udp}], any()) -> #{binary() := map()}).
map_ports(L, PortMapValue) ->
  P = lists:map(fun({Port, Type}) ->
    T = case Type of
          tcp -> <<"/tcp">>;
          udp -> <<"/udp">>
        end,
    PortBinary = erlang:integer_to_binary(Port),
    {<<PortBinary/binary, T/binary>>, PortMapValue}
                end, L),
  proplists:to_map(P).