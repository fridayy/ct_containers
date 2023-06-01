%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2022, leftshift.one software gmbh
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ct_containers_network).

-author("benjamin.krenn").

-behaviour(gen_server).

-include("ct_containers.hrl").
-include_lib("kernel/include/logger.hrl").

-export([start_link/1, delete/1, create/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state,
        {container_engine_module :: module(),
         network_name :: atom(),
         labels :: labels(),
         network_id :: term() | undefined}).

%%%--------------------------------------------------------------------------
%%% public API
%%%--------------------------------------------------------------------------
-spec start_link(ct_container_context()) -> gen:start_ret().
start_link(#{network := {NetworkName, _},
             labels := Labels,
             container_engine_module := CeMod}) ->
    gen_server:start_link({local, NetworkName}, ?MODULE, [NetworkName, Labels, CeMod], []).

create(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, create_network).

-spec delete(atom()) -> ok.
delete(NetworkName) when is_atom(NetworkName) ->
    gen_server:call(NetworkName, delete);
delete(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, delete).

%%%--------------------------------------------------------------------------
%%% callbacks
%%%--------------------------------------------------------------------------
init([NetworkName, Labels, CeMod]) ->
    {ok,
     #state{network_name = NetworkName,
            labels = Labels,
            container_engine_module = CeMod}}.

handle_call(create_network,
            _From,
            #state{network_name = NetworkName,
                   labels = Labels,
                   container_engine_module = CeMod} =
                State) ->
    logger:info("creating network '~p'", [NetworkName]),
    {ok, NetworkId} = CeMod:create_network(NetworkName, Labels),
    logger:info("network '~p' created", [NetworkName]),
    {reply, {ok, NetworkId}, State#state{network_id = NetworkId}};
handle_call(delete, _From, #state{network_name = ?DEFAULT_NETWORK_NAME
                             } = State) ->
    ?LOG_INFO(#{what => "skipping_deletion", info => #{"reason" => "default_network"}}),
    {stop, normal, ok, State};

handle_call(delete,
            _From,
            #state{container_engine_module = CeMod,
                   network_name = NetworkName,
                   network_id = NetworkId} =
                State) ->
    logger:info("deleting network '~p'", [NetworkName]),
    CeMod:delete_network(NetworkId),
    logger:info("network '~p' deleted", [NetworkName]),
    {stop, normal, ok, State};
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

%%%--------------------------------------------------------------------------
%%% private
%%%--------------------------------------------------------------------------
