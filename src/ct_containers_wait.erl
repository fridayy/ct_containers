%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%% This module contains all out of the box supported container wait strategies.
%%% @end
%%% Created : 01. Oct 2021 11:14 PM
%%%-------------------------------------------------------------------
-module(ct_containers_wait).

-author("bnjm").

%% API
-export([regex/1, passthrough/0]).

regex(Pattern) when is_list(Pattern) ->
    {ok, CompiledPattern} = re:compile(Pattern),
    fun(ContainerId, ContainerEngineModule, WatchContext) ->
        {ok, Logs} = ContainerEngineModule:container_logs(ContainerId),
        case re:run(Logs, CompiledPattern) of
            {match, _} ->
                {true, WatchContext};
            _Else ->
                {false, WatchContext}
        end
    end.

passthrough() ->
    fun(_ContainerId, _ContainerEngineModule, WatchContext) -> {true, WatchContext} end.
