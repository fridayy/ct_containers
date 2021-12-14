%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ct_containers_reaper).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, handle_continue/2]).

-define(SERVER, ?MODULE).
-define(RYUK_IMAGE, "ct_containers/ryuk:1").

-record(state, {socket}).


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}, {continue, start}}.

handle_continue(start, State) ->
  {ok, Pid} = ct_containers:start(?RYUK_IMAGE, [
    {wait_strategy, ct_containers_wait:regex(".*ryuk tcp listen*.")},
    {ports, [{8999, tcp}]},
    {timeout, 20000},
    {volumes, [<<"/var/run/docker.sock:/var/run/docker.sock">>]}
  ]),
  Host = ct_containers:host(Pid),
  {ok, MappedPort} = ct_containers:port(Pid, {8999, tcp}),
  logger:info("connecting to ryuk on ~p:~p", [Host, MappedPort]),
  {ok, Socket} = gen_tcp:connect(Host, MappedPort, [binary, {packet, 0}]),
  {noreply, State#state{socket = Socket}}.

handle_info({tcp, Port, <<"PING", $\n>>}, #state{socket = Socket} = State) ->
  logger:info("reaper received ping"),
  ok = gen_tcp:send(Socket, <<"PONG", $\n>>),
  {noreply, State};

handle_info(Unknown, State = #state{}) ->
  logger:warning("received unknown message ~p", [Unknown]),
  {noreply, State}.

handle_call(_Request, _From, State = #state{}) ->
{reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
{noreply, State}.


terminate(_Reason, _State = #state{socket = Socket}) ->
  gen_tcp:close(Socket),
  lager:info("closed"),
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.
