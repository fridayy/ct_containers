%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ct_containers_docker).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([create_container/1, start_container/1, stop_container/1, delete_container/1, container_logs/1, inspect/1, status/1, ip/1, port/2]).

-type(container_info() :: map()).

-define(SERVER, ?MODULE).
-define(DOCKER_SOCKET, "/var/run/docker.sock").

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(create_container(ct_containers_container:ct_container_spec()) -> {ok, binary()}).
create_container(ContainerSpec) ->
  gen_server:call(?SERVER, {create_container, ContainerSpec}).

-spec(start_container(binary()) -> {ok, binary()}).
start_container(ContainerId) ->
  gen_server:call(?SERVER, {start_container, ContainerId}).

-spec(stop_container(binary()) -> {ok, binary()}).
stop_container(ContainerId) ->
  gen_server:call(?SERVER, {stop_container, ContainerId}).

-spec(delete_container(binary()) -> {ok, binary()}).
delete_container(ContainerId) ->
  gen_server:call(?SERVER, {delete_container, ContainerId}).

-spec(inspect(binary()) -> {ok, container_info()}).
inspect(ContainerId) ->
  gen_server:call(?SERVER, {inspect, ContainerId}).

container_logs(ContainerId) ->
  gen_server:call(?SERVER, {container_logs, ContainerId}).

%%% @doc
%%% Extracts the status information from a ContainerInfo acquired by inspect/1
%%% @end
-spec(status(container_info()) -> {ok, binary()}).
status(ContainerInfo) ->
  #{<<"State">> := #{<<"Status">> := Status}} = ContainerInfo,
  {ok, Status}.

%%% @doc
%%% Reads the ip address from a ContainerInfo acquired by inspect/1
-spec(ip(container_info()) -> {ok, binary()}).
ip(ContainerInfo) ->
  #{<<"NetworkSettings">> := #{<<"IPAddress">> := IpAddress}} = ContainerInfo,
  {ok, IpAddress}.

-spec(port(ct_containers:port_mapping(), container_info()) -> {ok, 1..65565}).
port({Port, tcp}, ContainerInfo) ->
  #{<<"NetworkSettings">> := #{<<"Ports">> := PortMappings}} = ContainerInfo,
  PortBinary = erlang:integer_to_binary(Port),
  TcpBinary = erlang:atom_to_binary(tcp),
  PortMappingsKey = <<PortBinary/binary, "/" ,TcpBinary/binary>>,
  case maps:is_key(PortMappingsKey, PortMappings) of
    false -> {error, no_port};
    true ->
      [#{<<"HostPort">> := MappedPort} | _] = maps:get(PortMappingsKey, PortMappings),
      {ok, erlang:binary_to_integer(MappedPort)}
  end.

init([]) ->
  {ok, #state{}}.

handle_call({create_container, ContainerSpec}, _From, State = #state{}) ->
  Url = docker_url(<<"/containers/create">>),
  #{image := Image, port_mapping := PortMapping} = ContainerSpec,
  DockerContainerSpec = #{
    <<"Image">> => Image,
    <<"Labels">> => #{
      <<"com.github.ct_containers.managed">> => <<"true">>
    },
    <<"ExposedPorts">> => map_ports(PortMapping, #{}),
    <<"HostConfig">> => #{
      <<"PortBindings">> => map_ports(PortMapping, [#{<<"HostPort">> => <<"">>}])
    }
  },
  {201, #{<<"Id">> := ContainerId}} = ct_containers_http:post(Url, DockerContainerSpec),
  logger:info(#{what => "docker_engine_container_created"}),
  {reply, {ok, ContainerId}, State};

handle_call({start_container, ContainerId}, _From, State = #state{}) ->
  Url = docker_url(<<"/containers/", ContainerId/binary, "/start">>),
  {204, _} = ct_containers_http:post(Url, #{}),
  logger:info(#{what => "docker_engine_container_started"}),
  {reply, {ok, ContainerId}, State};

handle_call({stop_container, ContainerId}, _From, State = #state{}) ->
  Url = docker_url(<<"/containers/", ContainerId/binary, "/stop">>),
  case ct_containers_http:post(Url, #{}) of
    {204, _} ->
      logger:info(#{what => "docker_engine_container_stopped"}),
      {reply, {ok, ContainerId}, State};
    {304, _} ->
      logger:warning(#{what => "docker_engine_container_already_stopped", action => "skipping"}),
      {reply, {ok, ContainerId}, State}
  end;

handle_call({delete_container, ContainerId}, _From, State = #state{}) ->
  Url = docker_url(<<"/containers/", ContainerId/binary>>),
  {204, _} = ct_containers_http:delete(Url),
  logger:info(#{what => "docker_engine_container_deleted"}),
  {reply, {ok, ContainerId}, State};

handle_call({inspect, ContainerId}, _From, State) ->
  Url = docker_url(<<"/containers/", ContainerId/binary, "/json">>),
  {200, ContainerInfo} = ct_containers_http:get(Url),
  logger:info(#{what => "docker_engine_container_inspected"}),
  {reply, {ok, ContainerInfo}, State};

handle_call({container_logs, ContainerId}, _From, State = #state{}) ->
  Url = docker_url(<<"/containers/", ContainerId/binary, "/logs?stdout=true&stderr=true">>),
  {ok, Logs} = ct_containers_http:get_plain(Url),
  logger:info(#{what => "docker_engine_container_logs_read"}),
  {reply, {ok, Logs}, State};

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

handle_info(_Info, State = #state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%% private

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