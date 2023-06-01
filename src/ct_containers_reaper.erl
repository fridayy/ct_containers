%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ct_containers_reaper).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([
    reap_containers/0
]).

-include("ct_containers.hrl").
-include_lib("kernel/include/logger.hrl").

-define(RYUK_IMAGE, "testcontainers/ryuk:0.5.1").
-define(RYUK_PORT, 8080).

-record(state, {
    socket,
    from
}).

%% api

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc
%% Initiates the teardown of containers using ryuk.
%% @end
reap_containers() ->
    gen_server:call(?MODULE, reap, 10000).

%% gen server callbacks

init([]) ->
    {ok, Pid} =
        ct_containers:start(
            ?RYUK_IMAGE,
            [
                {wait_strategy, ct_containers_wait:regex(".*Started!*.")},
                {ports, [{?RYUK_PORT, tcp}]},
                {env, #{
                    <<"RYUK_VERBOSE">> => <<"true">>
                }},
                {timeout, 20000},
                {volumes, [<<"/var/run/docker.sock:/var/run/docker.sock">>]}
            ]
        ),

    Host = ct_containers:host(Pid),
    {ok, MappedPort} = ct_containers:port(Pid, {?RYUK_PORT, tcp}),
    {ok, Socket} = gen_tcp:connect(Host, MappedPort, [binary, {active, true}, {packet, line}]),
    {ok, #state{socket = Socket}}.

handle_info({tcp_closed, _Port}, State) ->
    ?LOG_WARNING("ryuk container stopped"),
    {stop, normal, State};
handle_info({tcp, Socket, <<"ACK\n">>}, #state{socket = Socket, from = From} = State) ->
    ok = gen_tcp:close(Socket),
    gen_server:reply(From, ok),
    {noreply, State};
handle_info(Unknown, State = #state{}) ->
    logger:warning("received unknown message ~p", [Unknown]),
    {noreply, State}.

handle_call(reap, From, #state{socket = Socket} = State) when Socket =/= undefined ->
    ?LOG_WARNING(#{what => "reaping_containers"}),
    Label = ?CT_CONTAINERS_LABEL,
    ok = gen_tcp:send(Socket, <<"label=", Label/binary, "\n">>),
    {noreply, State#state{from = From}};
handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(Request, State = #state{}) ->
    ?LOG_WARNING(#{what => "unhandled_cast", info => #{"message" => Request}}),
    {noreply, State}.

terminate(_Reason, _State = #state{socket = undefined}) ->
    ?LOG_INFO("terminated"),
    ok;
terminate(_Reason, _State) ->
    ?LOG_INFO("closed and terminated"),
    ok.
