%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ct_containers_container_sup).

-behaviour(supervisor).

-include("ct_containers.hrl").

-type start_container_req() :: #{image => binary()}.

-export_type([start_container_req/0]).

-export([start_link/0, init/1, start_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(ct_container_context()) -> supervisor:startchild_ret().
start_child(Context) ->
    supervisor:start_child(?MODULE, [Context]).

init([]) ->
    ContainerStatemSpec =
        #{
            id => ct_containers_container,
            start => {ct_containers_container, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker
        },

    {ok,
        {
            #{
                strategy => simple_one_for_one,
                intensity => 1,
                period => 1
            },
            [ContainerStatemSpec]
        }}.
