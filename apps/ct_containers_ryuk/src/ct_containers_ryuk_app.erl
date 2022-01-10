%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one software gmbh
%% @doc ct_containers_ryuk application
%% @end
%%%-------------------------------------------------------------------

-module(ct_containers_ryuk_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, LSock} = gen_tcp:listen(8999, [binary, {packet, 0}]),
    logger:info("ryuk tcp listen"),
    ct_containers_ryuk_sup:start_link(LSock).

stop(_State) ->
    ok.
