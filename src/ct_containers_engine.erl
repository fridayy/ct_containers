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

-callback create_container(ct_container_context()) -> {ok, container_id()}.
-callback start_container(container_id()) -> {ok, container_id()}.
-callback stop_container(container_id()) -> {ok, container_id()}.
-callback delete_container(container_id()) -> {ok, container_id()}.
-callback list_containers() -> {ok, list()}.
-callback list_containers([{filters, map()}] | []) -> {ok, list()}.
-callback container_logs(container_id()) -> {ok, binary()}.
-callback status(container_id()) -> {ok, binary()}.
-callback host(container_id()) -> {ok, binary()}.
-callback port(container_id(), port_mapping()) -> {ok, integer()}.
-callback create_network(network_name(), labels()) -> {ok, network_id()}.
-callback delete_network(network_id()) -> {ok, network_id()}.
-callback list_networks([{filters, map()}] | []) -> {ok, list()}.
