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
      logger:info("ok ~p", [Reason]),
      reap(#{label => [<<?CT_CONTAINERS_LABEL/binary, "=", "true">>]});
    {error, no_client} ->
      logger:error("no client connected")
  end,
  logger:info("done - goodbye cruel world"),
  ok.

reap(Filters) ->
  logger:info("reaping containers with label: ~p", [Filters]),
  {ok, Containers} = ct_containers_docker:list([{filters, Filters}]),
  ContainerIds = lists:map(fun(#{<<"Id">> := ContainerId}) -> ContainerId end, Containers),
  lists:foreach(fun(Id) ->
    ct_containers_docker:stop_container(Id),
    logger:info("stopped ~p", [Id])
                end, ContainerIds),
  ok.