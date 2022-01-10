%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ct_containers_ryuk_srv).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([start/1, start/0]).

-define(SERVER, ?MODULE).
-define(CLIENT_CONNECT_TIMEOUT_MS, 10000).
-define(CLIENT_MESSAGE_TIMEOUT_MS, 5000).
-define(INTERVAL_MS, 2000).

-record(state, {listener_socket, client_socket, reply_to}).

%% @doc
%% Starts the ryuk mechanism with the given fixed delay. After the
%% the given fixed delay start returns either:
%% <ul>
%% <li> {ok, client_disconnected}: if the client stopped responding to pings</li>
%% <li> {ok, delay_reached}: the given fixed delay time has been reached</li>
%% <li> {error, no_client}: no client connected within the configured timeout</li>
%%
%% Note that the delay starts <b>after</b> a client successfully connected.
%% @end
start(FixedDelay) ->
    gen_server:call(?SERVER,
                    {start, FixedDelay},
                    FixedDelay + ?CLIENT_CONNECT_TIMEOUT_MS + 5000).

start() ->
    gen_server:call(?SERVER, {start, infinity}, infinity).

start_link(LSock) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [LSock], []).

init([LSock]) ->
    {ok, #state{listener_socket = LSock}}.

handle_info(timeout, #state{listener_socket = LSock, reply_to = From} = State) ->
    logger:warning("client did not respond to ping in time"),
    gen_server:reply(From, {ok, client_disconnected}),
    gen_tcp:close(LSock),
    {stop, normal, State};
handle_info(ping, #state{client_socket = CSock} = State) ->
    logger:info("sending ping"),
    gen_tcp:send(CSock, <<"PING", $\n>>),
    {noreply, State, ?CLIENT_MESSAGE_TIMEOUT_MS};
handle_info(kill, #state{listener_socket = LSock, reply_to = From} = State) ->
    gen_server:reply(From, {ok, delay_reached}),
    gen_tcp:close(LSock),
    {noreply, State};
handle_info({tcp, Port, <<"PONG", $\n>>}, State) ->
    logger:debug("pong received from", [Port]),
    timer:send_after(?INTERVAL_MS, self(), ping),
    {noreply, State};
handle_info({tcp_closed, _Port},
            #state{listener_socket = LSock, reply_to = From} = State) ->
    gen_server:reply(From, {ok, client_disconnected}),
    gen_tcp:close(LSock),
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call({start, FixedDelay}, From, State = #state{listener_socket = LSock}) ->
    logger:info("waiting for client to connect"),
    case gen_tcp:accept(LSock, ?CLIENT_CONNECT_TIMEOUT_MS) of
        {ok, CSock} ->
            case FixedDelay of
                infinity ->
                    logger:info("using no fixed delay");
                _ ->
                    logger:info("using fixed delay ~p ms", [FixedDelay]),
                    timer:send_after(FixedDelay, self(), kill)
            end,
            timer:send_after(?INTERVAL_MS, self(), ping),
            {noreply, State#state{client_socket = CSock, reply_to = From}};
        {error, timeout} ->
            logger:error("no client connected within the specified timeout"),
            gen_tcp:close(LSock),
            {stop, normal, {error, no_client}, State}
    end;
handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.
