%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ct_containers_ryuk_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(LSock) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LSock]).

init([LSock]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 5,
          period => 30},

    ChildSpecs =
        [#{id => ct_containers_ryuk_srv,
           start => {ct_containers_ryuk_srv, start_link, [LSock]},
           restart => temporary,
           shutdown => 2000,
           type => worker,
           modules => [ct_containers_ryuk_srv]}],

    {ok, {SupFlags, ChildSpecs}}.
