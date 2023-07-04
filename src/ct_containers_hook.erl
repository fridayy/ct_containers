%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%% This module provides the ct_containers common test hook implemenation.
%%% <a href="https://www.erlang.org/doc/man/ct_hooks.html">More information on ct hooks</a>
%%% @end
%%% Created : 08. Dec 2021 8:37 PM
%%%-------------------------------------------------------------------
-module(ct_containers_hook).

-author("bnjm").

%% API
-export([
    pre_init_per_suite/3,
    post_end_per_suite/4,
    init/2,
    pre_init_per_testcase/4,
    pre_init_per_group/4,
    post_end_per_group/5,
    post_end_per_testcase/5
]).

-type ct_lifecycle() :: group | suite | testcase.

%% general hook options
-type ct_hook_spec() ::
    #{lifecycle_per => ct_lifecycle(), containers => [ct_hook_container_spec()]}.

%% container definitions

-type ct_container_name() :: atom().
-type ct_hook_container_spec() ::
    #{
        name => ct_container_name(),
        image => string(),
        options => list()
    }.

-export_type([ct_hook_spec/0, ct_hook_container_spec/0]).

-record(state, {container_defs = [], active_containers = [], lifecycle = suite :: ct_lifecycle()}).

-spec init(any(), [ct_hook_spec()]) -> {ok, #state{}}.
init(_Id, [#{lifecycle_per := Lifecycle, containers := ContainerDefs}]) ->
    {ok, _Apps} = application:ensure_all_started(ct_containers),
    {ok, #state{container_defs = ContainerDefs, lifecycle = Lifecycle}};
init(_Id, [#{containers := ContainerDefs}]) ->
    {ok, _Apps} = application:ensure_all_started(ct_containers),
    {ok, #state{container_defs = ContainerDefs}}.

pre_init_per_suite(
    _SuiteName,
    Config,
    #state{container_defs = ContainerDefs, lifecycle = suite} = State
) ->
    do_init(ContainerDefs, Config, State);
pre_init_per_suite(_SuiteName, Config, State) ->
    {Config, State}.

pre_init_per_group(
    _SuiteName,
    _GroupName,
    Config,
    #state{container_defs = ContainerDefs, lifecycle = group} = State
) ->
    do_init(ContainerDefs, Config, State);
pre_init_per_group(_SuiteName, _GroupName, Config, State) ->
    {Config, State}.

pre_init_per_testcase(
    _SuiteName,
    _TestCase,
    Config,
    #state{container_defs = ContainerDefs, lifecycle = testcase} = State
) ->
    do_init(ContainerDefs, Config, State);
pre_init_per_testcase(_SuiteName, _TestCase, Config, State) ->
    {Config, State}.

post_end_per_suite(
    _SuiteName,
    _Config,
    Return,
    #state{active_containers = ActiveContainers, lifecycle = suite} = State
) ->
    stop_containers(ActiveContainers),
    {Return, State};
post_end_per_suite(_SuiteName, _Config, Return, State) ->
    {Return, State}.

post_end_per_group(
    _SuiteName,
    _GroupName,
    _Config,
    Return,
    #state{active_containers = ActiveContainers, lifecycle = group} = State
) ->
    ok = stop_containers(ActiveContainers),
    {Return, State};
post_end_per_group(_SuiteName, _GroupName, _Config, Return, State) ->
    {Return, State}.

post_end_per_testcase(
    _SuiteName,
    _TestCase,
    _Config,
    Return,
    #state{active_containers = ActiveContainers, lifecycle = testcase} = State
) ->
    ok = stop_containers(ActiveContainers),
    {Return, State};
post_end_per_testcase(_SuiteName, _TestCase, _Config, Return, State) ->
    {Return, State}.

do_init(ContainerDefs, Config, State) ->
    ContainersByName = start_containers(ContainerDefs),
    {[{ct_containers, ContainersByName} | Config], State#state{active_containers = ContainersByName}}.

-spec start_containers([ct_hook_container_spec()]) -> #{ct_container_name() => pid()}.
start_containers(ContainerDefs) ->
    Containers =
        lists:map(
            fun(M) ->
                #{
                    name := Name,
                    image := Image,
                    options := Opts
                } =
                    M,
                ct:log("Starting container [~p]", [Image]),
                {ok, Pid} = ct_containers:start(Image, Opts),
                {Name, Pid}
            end,
            ContainerDefs
        ),
    maps:from_list(Containers).

stop_containers(ActiveContainers) ->
    maps:foreach(
        fun(Name, Pid) ->
            ct:log("Stopping container ~p [~p]", [Name, Pid]),
            ct_containers:stop(Pid)
        end,
        ActiveContainers
    ),
    ok.
