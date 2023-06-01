%%%-------------------------------------------------------------------
%%% @author benjamin.krenn
%%% @copyright (C) 2021, leftshift.one software gmbh
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2021 9:28 PM
%%%-------------------------------------------------------------------
-module(ct_containers_http).

-author("benjamin.krenn").

%% API
-export([get/1, post/2, delete/1, get_plain/1, url_encode/1]).

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

url_encode(Url) ->
    hackney_url:urlencode(Url).
