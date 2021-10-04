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
-export([create_container/1, start_container/1, stop_container/1, delete_container/1, container_status/1, container_logs/1]).

-define(SERVER, ?MODULE).
-define(DOCKER_SOCKET, "/var/run/docker.sock").

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(create_container(map()) -> {ok, binary()}).
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

-spec(container_status(binary()) -> {ok, binary()}).
container_status(ContainerId) ->
  gen_server:call(?SERVER, {container_status, ContainerId}).

container_logs(ContainerId) ->
  gen_server:call(?SERVER, {container_logs, ContainerId}).

init([]) ->
  {ok, #state{}}.


handle_call({create_container, ContainerSpec}, _From, State = #state{}) ->
  Url = docker_url(<<"/containers/create">>),
  {201, #{<<"Id">> := ContainerId}} = ct_containers_http:post(Url, ContainerSpec),
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

handle_call({container_status, ContainerId}, _From, State = #state{}) ->
  Url = docker_url(<<"/containers/", ContainerId/binary, "/json">>),
  {200, #{<<"State">> := #{<<"Status">> := Status}}} = ct_containers_http:get(Url),
  logger:info(#{what => "docker_engine_container_status_read"}),
  {reply, {ok, Status}, State};

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