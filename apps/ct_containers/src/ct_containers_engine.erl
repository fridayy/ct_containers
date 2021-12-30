%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one Software GmbH
%%% @doc
%%%
%%% @end
%%% Created : 30. Dec 2021 11:52 PM
%%%-------------------------------------------------------------------
-module(ct_containers_engine).

-author("benjamin.krenn").

-include("ct_containers.hrl").

-callback create_container(ct_container_spec()) -> {ok, container_id()}.

-callback start_container(container_id()) -> {ok, container_id()}.

-callback stop_container(container_id()) -> {ok, container_id()}.

-callback delete_container(container_id()) -> {ok, container_id()}.

-callback container_logs(container_id()) -> {ok, string()}.

-callback status(container_id()) -> {ok, binary()}.

-callback host(container_id()) -> {ok, binary()}.

-callback port(container_id(), port_mapping()) -> {ok, integer()}.