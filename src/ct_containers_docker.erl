%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%% ct_containers_engine - Docker implementation.
%%% @end
%%%-------------------------------------------------------------------
-module(ct_containers_docker).

-behaviour(ct_containers_engine).

-include("ct_containers.hrl").

-export([
    create_container/1,
    start_container/1,
    stop_container/1,
    delete_container/1,
    container_logs/1,
    inspect/1,
    status/1,
    host/1,
    port/2,
    pull_image/1,
    list_containers/0,
    list_containers/1,
    delete_network/1,
    create_network/2,
    list_networks/1,
    detach_container/2,
    prune_networks/0
]).

-define(DOCKER_SOCKET_DEFAULT, "/var/run/docker.sock").
-define(DOCKER_HOST_ENV, "DOCKER_HOST").

-spec create_container(ct_containers_container:ct_container_context()) -> {ok, binary()}.
create_container(ContainerSpec) ->
    Url = docker_url(<<"/containers/create">>),
    Image = maps:get(image, ContainerSpec),
    DockerContainerSpec = map_container_spec(ContainerSpec),
    case ct_containers_http:post(Url, DockerContainerSpec) of
        {201, #{<<"Id">> := ContainerId}} ->
            logger:info(#{what => "docker_engine_container_created"}),
            {ok, ContainerId};
        {404, _} ->
            logger:info(#{what => "docker_engine_container_pull"}),
            ok = pull_image(Image),
            create_container(ContainerSpec)
    end.

pull_image(Image) when is_binary(Image) ->
    Url = docker_url(<<"/images/create?fromImage=", Image/binary>>),
    {200, _} = ct_containers_http:post_plain(Url, #{}),
    ok.

-spec start_container(binary()) -> {ok, binary()}.
start_container(ContainerId) ->
    Url = docker_url(<<"/containers/", ContainerId/binary, "/start">>),
    {204, _} = ct_containers_http:post_plain(Url, #{}),
    logger:info(#{what => "docker_engine_container_started"}),
    {ok, ContainerId}.

-spec stop_container(binary()) -> {ok, binary()}.
stop_container(ContainerId) ->
    Url = docker_url(<<"/containers/", ContainerId/binary, "/stop">>),
    case ct_containers_http:post_plain(Url, #{}) of
        {204, _} ->
            logger:info(#{what => "docker_engine_container_stopped"}),
            {ok, ContainerId};
        {304, _} ->
            logger:warning(#{
                what => "docker_engine_container_already_stopped",
                action => "skipping"
            }),
            {ok, ContainerId}
    end.

-spec delete_container(binary()) -> {ok, binary()}.
delete_container(ContainerId) ->
    Url = docker_url(<<"/containers/", ContainerId/binary>>),
    {204, _} = ct_containers_http:delete_plain(Url),
    logger:info(#{what => "docker_engine_container_deleted"}),
    {ok, ContainerId}.

-spec inspect(binary()) -> {ok, map()}.
inspect(ContainerId) ->
    Url = docker_url(<<"/containers/", ContainerId/binary, "/json">>),
    {200, ContainerInfo} = ct_containers_http:get(Url),
    logger:debug(#{what => "docker_engine_container_inspected"}),
    {ok, ContainerInfo}.

list_containers() ->
    list_containers([]).

-spec list_containers([{filters, map()}] | []) -> {ok, list()}.
list_containers([{filters, Filters}]) ->
    do_list(<<"/containers/json">>, Filters);
list_containers([]) ->
    do_list(<<"/containers/json">>, #{}).

-spec container_logs(binary()) -> {ok, binary()}.
container_logs(ContainerId) ->
    Url = docker_url(<<"/containers/", ContainerId/binary, "/logs?stdout=true&stderr=true">>),
    {ok, Logs} = ct_containers_http:get_plain(Url),
    logger:info(#{what => "docker_engine_container_logs_read"}),
    {ok, Logs}.

%%% @doc
%%% Extracts the status information from a ContainerInfo acquired by inspect/1
%%% @end
-spec status(container_id()) -> {ok, binary()}.
status(ContainerId) ->
    {ok, ContainerInfo} = inspect(ContainerId),
    #{<<"State">> := #{<<"Status">> := Status}} = ContainerInfo,
    {ok, Status}.

%%% @doc
%%% Reads the host address from a ContainerInfo acquired by inspect/1
-spec host(container_id()) -> {ok, binary()}.
host(ContainerId) ->
    {ok, ContainerInfo} = inspect(ContainerId),
    case running_in_container() of
        false ->
            {ok, <<"localhost">>};
        true ->
            #{<<"NetworkSettings">> := #{<<"Networks">> := Networks}} = ContainerInfo,
            NetworksIterator = maps:iterator(Networks),
            {_Key, Network, _NextIterator} = maps:next(NetworksIterator),
            #{<<"Gateway">> := IpAddress} = Network,
            {ok, IpAddress}
    end.

-spec port(container_id(), port_mapping()) -> {ok, 1..65565}.
port(ContainerId, {Port, tcp}) ->
    {ok, ContainerInfo} = inspect(ContainerId),
    #{<<"NetworkSettings">> := #{<<"Ports">> := PortMappings}} = ContainerInfo,
    PortBinary = erlang:integer_to_binary(Port),
    TcpBinary = erlang:atom_to_binary(tcp),
    PortMappingsKey = <<PortBinary/binary, "/", TcpBinary/binary>>,
    case maps:is_key(PortMappingsKey, PortMappings) of
        false ->
            {error, no_port};
        true ->
            [#{<<"HostPort">> := MappedPort} | _] = maps:get(PortMappingsKey, PortMappings),
            if
                MappedPort =:= null ->
                    {error, no_port};
                true ->
                    {ok, erlang:binary_to_integer(MappedPort)}
            end
    end.

create_network(Name, Labels) ->
    Url = docker_url(<<"/networks/create">>),
    BinaryName = atom_to_binary(Name),
    case
        ct_containers_http:post(
            Url,
            #{
                <<"Name">> => BinaryName,
                <<"CheckDuplicate">> => true,
                <<"Labels">> => Labels
            }
        )
    of
        {201, #{<<"Id">> := NetworkId}} ->
            {ok, NetworkId};
        {409, _} ->
            logger:info("network already exists"),
            {200, #{<<"Id">> := NetworkId}} =
                ct_containers_http:get(docker_url(<<"/networks/", BinaryName/binary>>)),
            {ok, NetworkId}
    end.

delete_network(Identifier) when is_binary(Identifier) ->
    Url = docker_url(<<"/networks/", Identifier/binary>>),
    {204, _} = ct_containers_http:delete_plain(Url),
    {ok, Identifier}.

prune_networks() ->
    Url = docker_url(<<"/networks/prune">>),
    {200, _} = ct_containers_http:post_plain(Url, <<>>),
    ok.

detach_container(NetworkId, ContainerName) when is_binary(NetworkId) and is_binary(ContainerName) ->
    Url = docker_url(<<"/networks/", NetworkId/binary, "/disconnect">>),
    {200, _} = ct_containers_http:post_plain(Url, #{
        <<"Container">> => ContainerName,
        <<"Force">> => true
    }),
    {ok, ContainerName}.

list_networks([{filters, Filters}]) ->
    do_list(<<"/networks">>, Filters).

%% private
running_in_container() ->
    filelib:is_file("/.dockerenv").

-spec docker_url(binary()) -> binary().
docker_url(Path) ->
    DockerHostEnv = os:getenv(?DOCKER_HOST_ENV),
    case parse_docker_host_env(DockerHostEnv) of
        false ->
            UrlEncodedSocketLocation = ct_containers_http:url_encode(?DOCKER_SOCKET_DEFAULT),
            <<"http+unix://", UrlEncodedSocketLocation/binary, Path/binary>>;
        Else ->
            <<Else/binary, Path/binary>>
    end.

%%% @doc
%%% maps ports from the ct_containers format to the docker format
%%% @end
-spec map_ports([{number(), tcp | udp}], any()) -> #{binary() := map()}.
map_ports(L, PortMapValue) ->
    P = lists:map(
        fun({Port, Type}) ->
            T =
                case Type of
                    tcp ->
                        <<"/tcp">>;
                    udp ->
                        <<"/udp">>
                end,
            PortBinary = erlang:integer_to_binary(Port),
            {<<PortBinary/binary, T/binary>>, PortMapValue}
        end,
        L
    ),
    proplists:to_map(P).

do_list(Url, #{}) ->
    {200, List} = ct_containers_http:get(docker_url(Url)),
    {ok, List};
do_list(Url, Filters) ->
    BFilters = jsone:encode(Filters),
    EncodedFilters = ct_containers_http:url_encode(BFilters),
    UrlWithFilter = docker_url(<<Url/binary, "?filters=", EncodedFilters/binary>>),
    {200, List} = ct_containers_http:get(UrlWithFilter),
    {ok, List}.

%%% @doc
%%% Turns a ct container spec into a specification understood by the docker api
%%% @end
-spec map_container_spec(ct_container_context()) -> map().
map_container_spec(#{
    image := Image,
    port_mapping := PortMapping,
    labels := Labels,
    binds := Binds,
    env := Env,
    network := {Network, Alias}
}) when is_list(Alias) ->
    NetworkBinary = erlang:atom_to_binary(Network),
    AliasBinary = erlang:list_to_binary(Alias),
    #{
        <<"Image">> => Image,
        <<"Labels">> => Labels,
        <<"Env">> => map_env(Env),
        <<"ExposedPorts">> => map_ports(PortMapping, #{}),
        <<"HostConfig">> =>
            #{
                <<"PortBindings">> => map_ports(PortMapping, [#{<<"HostPort">> => <<"">>}]),
                <<"NetworkMode">> => NetworkBinary,
                <<"Binds">> => Binds
            },
        <<"NetworkingConfig">> =>
            #{<<"EndpointsConfig">> => #{NetworkBinary => #{<<"Aliases">> => [AliasBinary]}}}
    };
map_container_spec(#{
    image := Image,
    port_mapping := PortMapping,
    labels := Labels,
    binds := Binds,
    env := Env
}) ->
    #{
        <<"Image">> => Image,
        <<"Labels">> => Labels,
        <<"ExposedPorts">> => map_ports(PortMapping, #{}),
        <<"Env">> => map_env(Env),
        <<"HostConfig">> =>
            #{
                <<"PortBindings">> => map_ports(PortMapping, [#{<<"HostPort">> => <<"">>}]),
                <<"Binds">> => Binds
            }
    }.

map_env(Env) ->
    maps:fold(fun(K, V, Acc) -> [<<K/binary, "=", V/binary>> | Acc] end, [], Env).

parse_docker_host_env(false) ->
    false;
parse_docker_host_env("http://" ++ _ = Url) ->
    Url;
parse_docker_host_env("tcp://" ++ R) ->
    Url = "http://" ++ R,
    erlang:list_to_binary(Url);
parse_docker_host_env(_) ->
    error(unsupported_scheme).
