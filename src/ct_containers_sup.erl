%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(ct_containers_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one,
    intensity => 1,
    period => 5},
  ChildSpecs = [
    #{
      id => ct_containers_container_sup,
      start => {ct_containers_container_sup, start_link, []},
      type => supervisor
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.
