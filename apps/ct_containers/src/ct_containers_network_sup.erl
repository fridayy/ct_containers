%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2022, leftshift.one software gmbh
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ct_containers_network_sup).

-author("benjamin.krenn").

-behaviour(supervisor).

-export([start_link/0, init/1, start_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(CtContainerSpec) ->
    case supervisor:start_child(?MODULE, [CtContainerSpec]) of
      {error, {already_started, Pid}} ->
        logger:info("network already created"),
        {ok, Pid};
      Else -> Else
    end.

init([]) ->
    NetworkSpec =
        #{id => ct_containers_network,
          start => {ct_containers_network, start_link, []},
          restart => temporary,
          shutdown => 2000,
          type => worker,
          modules => [ct_containers_network]},

    {ok,
     {#{strategy => simple_one_for_one,
        intensity => 5,
        period => 30},
      [NetworkSpec]}}.
