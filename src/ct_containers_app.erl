%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one software gmbh
%% @doc ct_containers public API
%% @end
%%%-------------------------------------------------------------------

-module(ct_containers_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

start(_StartType, _StartArgs) ->
    ?LOG_DEBUG(#{what => "ct_containers starting"}),
    ct_containers_sup:start_link().

stop(_State) ->
    ok.
