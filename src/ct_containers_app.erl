%%%-------------------------------------------------------------------
%% @doc ct_containers public API
%% @end
%%%-------------------------------------------------------------------

-module(ct_containers_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  ct_containers_sup:start_link().

stop(_State) ->
  ok.