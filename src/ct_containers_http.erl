%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2021 9:28 PM
%%%-------------------------------------------------------------------
-module(ct_containers_http).

-author("bnjm").

%% API
-export([get/1, post/2, delete/1, get_plain/1, url_encode/1, post_plain/2, delete_plain/1]).

-spec get(binary()) -> {300..500, map() | [map()]}.
get(Url) ->
    {ok, Status, _H, ClientRef} = hackney:get(<<Url/binary>>),
    {ok, ResponseBody} = hackney:body(ClientRef),
    case ResponseBody of
        <<>> ->
            {Status, #{}};
        B ->
            {Status, jsone:decode(B)}
    end.

%% @doc
%% Returns the plain response body without json decode
%% @end
-spec get_plain(binary()) -> {ok, binary()}.
get_plain(Url) ->
    {ok, _Status, _H, ClientRef} = hackney:get(<<Url/binary>>),
    hackney:body(ClientRef).

-spec post(binary(), #{binary() => binary()}) -> {integer(), map()}.
post(Url, Payload) ->
    EncodedPayload = jsone:encode(Payload),
    {ok, Status, _H, ClientRef} =
        hackney:request(
            post,
            <<Url/binary>>,
            [{<<"Content-Type">>, <<"application/json">>}],
            EncodedPayload,
            [{recv_timeout, 10000}]
        ),
    {ok, ResponseBody} = hackney:body(ClientRef),
    case ResponseBody of
        <<>> ->
            {Status, #{}};
        B ->
            {Status, jsone:decode(B)}
    end.

%% @doc
%% Do post and ignore the body.
%% @end
-spec post_plain(binary(), #{binary() => binary()}) -> {integer(), binary()}.
post_plain(Url, Payload) ->
    EncodedPayload = jsone:encode(Payload),
    {ok, Status, _H, ClientRef} =
        hackney:request(
            post,
            <<Url/binary>>,
            [{<<"Content-Type">>, <<"application/json">>}],
            EncodedPayload,
            [{recv_timeout, 10000}]
        ),
    {Status, hackney:body(ClientRef)}.

-spec delete(binary()) -> {integer(), map()}.
delete(Url) ->
    {ok, Status, _H, ClientRef} = hackney:delete(<<Url/binary>>),
    {ok, ResponseBody} = hackney:body(ClientRef),
    case ResponseBody of
        <<>> ->
            {Status, #{}};
        B ->
            {Status, jsone:decode(B)}
    end.

-spec delete_plain(binary()) -> {integer(), binary()}.
delete_plain(Url) ->
    {ok, Status, _H, ClientRef} = hackney:delete(<<Url/binary>>),
    {ok, ResponseBody} = hackney:body(ClientRef),
    case ResponseBody of
        <<>> ->
            {Status, #{}};
        B ->
            {Status, ResponseBody}
    end.

url_encode(Url) ->
    hackney_url:urlencode(Url).
