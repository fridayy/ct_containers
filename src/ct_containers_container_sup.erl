%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ct_containers_container_sup).

-behaviour(supervisor).


-type(start_container_req() :: #{image => binary()}).

-export_type([start_container_req/0]).
-export([start_link/0, init/1, start_child/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
  supervisor:start_child(?MODULE, []).

init([]) ->
  ContainerStatemSpec = #{id => 'ct_containers_container',
    start => {'ct_containers_container', start_link, [ct_containers_docker]},
    restart => temporary,
    shutdown => 5000,
    type => worker
  },

  {ok, {#{strategy => simple_one_for_one,
    intensity => 5,
    period => 30},
    [ContainerStatemSpec]}
  }.
