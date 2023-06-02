%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, leftshift.one Software GmbH
%%% @doc
%%% Uses https://github.com/testcontainers/moby-ryuk
%%% to reap containers in the unlikely event of vm crashes or other
%%% unexpected halt scenarios that prevent further code execution.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ct_containers_reaper).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("ct_containers.hrl").
-include_lib("kernel/include/logger.hrl").

-define(RYUK_IMAGE, "testcontainers/ryuk:0.5.1").
-define(RYUK_PORT, 8080).

-record(state, {
    socket
}).

%% api

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    Label = ?CT_CONTAINERS_LABEL,
    ok = gen_tcp:send(Socket, <<"label=", Label/binary, "\n">>),
    {ok, #state{socket = Socket}}.

handle_info({tcp_closed, _Port}, State) ->
    ?LOG_WARNING(#{what => "unexpected_ryuk_conn_close"}),
    {stop, normal, State};
handle_info({tcp, Socket, <<"ACK\n">>}, #state{socket = Socket} = State) ->
    ?LOG_DEBUG(#{what => "ryuk_ack_recv"}),
    {noreply, State};
handle_info(Unknown, State) ->
    ?LOG_WARNING(#{what => "unexpected_message", info => #{"message" => Unknown}}),
    {noreply, State}.

handle_call(Request, _From, State) ->
    ?LOG_WARNING(#{what => "unexpected_call", info => #{"message" => Request}}),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?LOG_WARNING(#{what => "unexpected_cast", info => #{"message" => Request}}),
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    ?LOG_DEBUG(#{what => "ryuk_terminate"}),
    gen_tcp:close(Socket),
    ok.
