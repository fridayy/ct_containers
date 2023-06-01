%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(ct_containers_sup).

-behaviour(supervisor).

-export([
    init/1,
    start_link/0
]).

-include("ct_containers.hrl").
-include_lib("kernel/include/logger.hrl").

-spec start_link() -> Result when
    Result :: supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags =
        #{
            strategy => one_for_one,
            intensity => 1,
            period => 5
        },
    ChildSpecs =
        [
            #{
                id => ct_containers_container_sup,
                start => {ct_containers_container_sup, start_link, []},
                type => supervisor
            },
            #{
                id => ct_containers_network_sup,
                start => {ct_containers_network_sup, start_link, []},
                type => supervisor
            }
        ],
    ActualChildSpecs = with_ryuk(is_ryuk_enabled(), ChildSpecs),
    {ok, {SupFlags, ActualChildSpecs}}.

with_ryuk(true, ChildSpecs) ->
    ChildSpecs ++
        [
            #{
                id => ct_containers_reaper,
                start => {ct_containers_reaper, start_link, []},
                type => worker,
                restart => transient
            }
        ];
with_ryuk(false, ChildSpecs) ->
    ChildSpecs.

is_ryuk_enabled() ->
    Enabled = application:get_env(?RYUK_DISABLED_APP_VAR, undefined),
    is_ryuk_enabled(Enabled).
is_ryuk_enabled(undefined) ->
    Enabled = os:getenv(?RYUK_DISABLED_ENV_VAR, undefined),
    case Enabled of
        undefined ->
            true;
        _ ->
            ?LOG_DEBUG(#{event => "ryuk_disabled"}),
            false
    end.
