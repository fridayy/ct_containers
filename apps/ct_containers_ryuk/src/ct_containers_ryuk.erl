%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2021 11:59 AM
%%%-------------------------------------------------------------------
-module(ct_containers_ryuk).

-author("bnjm").

-include_lib("ct_containers/include/ct_containers.hrl").

%% API
-export([main/1]).

main(_Args) ->
    {ok, _} = application:ensure_all_started(ct_containers_ryuk),
    logger:info("started"),
    case ct_containers_ryuk_srv:start() of
        {ok, Reason} ->
            %% if the delay has been reached or the connection to the host system has been killed
            logger:info("ok ~p", [Reason]),
            reap_containers(#{label => [<<?CT_CONTAINERS_LABEL/binary, "=", "true">>]}),
            reap_networks(#{label => [<<?CT_CONTAINERS_LABEL/binary, "=", "true">>]});
        {error, no_client} ->
            logger:error("no client connected")
    end,
    logger:info("done - goodbye cruel world"),
    ok.

reap_containers(Filters) ->
    logger:info("reaping containers with label: ~p", [Filters]),
    {ok, Containers} = ct_containers_docker:list_containers([{filters, Filters}]),
    ContainerIds = lists:map(fun(#{<<"Id">> := ContainerId}) -> ContainerId end, Containers),
    lists:foreach(fun(Id) ->
                     ct_containers_docker:stop_container(Id),
                     ct_containers_docker:delete_container(Id),
                     logger:info("stopped ~p", [Id])
                  end,
                  ContainerIds),
    ok.

%%TODO: merge reap_containers & networks
reap_networks(Filters) ->
    logger:info("reaping networks with label: ~p", [Filters]),
    {ok, Networks} = ct_containers_docker:list_networks([{filters, Filters}]),
    NetworkIds = lists:map(fun(#{<<"Id">> := NetworkId}) -> NetworkId end, Networks),
    lists:foreach(fun(Id) ->
                     ct_containers_docker:delete_network(Id),
                     logger:info("Deleted network ~p", [Id])
                  end,
                  NetworkIds),
    ok.
