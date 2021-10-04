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
-export([get/1, post/2, delete/1, get_plain/1]).

-spec(get(binary()) -> {300..500, map() | [map()]}).
get(Url) ->
  {ok, Status, _H, ClientRef} = hackney:get(<<Url/binary>>),
  {ok, ResponseBody} = hackney:body(ClientRef),
  case ResponseBody of
    <<>> -> {Status, #{}};
    B -> {Status, jsone:decode(B)}
  end.

%% @private
%% @doc Returns the plain response body without json decode
get_plain(Url) ->
  {ok, _Status, _H, ClientRef} = hackney:get(<<Url/binary>>),
  hackney:body(ClientRef).

post(Url, Payload) ->
  EncodedPayload = jsone:encode(Payload),
  {ok, Status, _H, ClientRef} = hackney:request(post, <<Url/binary>>,
    [{<<"Content-Type">>, <<"application/json">>}],
    EncodedPayload),
  {ok, ResponseBody} = hackney:body(ClientRef),
  case ResponseBody of
    <<>> -> {Status, #{}};
    B -> {Status, jsone:decode(B)}
  end.

delete(Url) ->
  {ok, Status, _H, ClientRef} = hackney:delete(<<Url/binary>>),
  {ok, ResponseBody} = hackney:body(ClientRef),
  case ResponseBody of
    <<>> -> {Status, #{}};
    B -> {Status, jsone:decode(B)}
  end.


