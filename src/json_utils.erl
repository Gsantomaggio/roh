%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Oct 2017 11:44
%%%-------------------------------------------------------------------
-module(json_utils).
-author("gabriele").

%% API
-export([to_json_array/2, try_decode/1, decode/1, try_decode/2, encode/1]).


to_json_array(L, Fun) ->
    [[[Fun(X) | <<",">>] || X <- L, X =/= lists:last(L)] | Fun(lists:last(L))].


try_decode(Value) ->
    try_decode(Value, []).


try_decode(Value, Opts) ->
    try
        {ok, decode(Value, Opts)}
    catch error: Reason ->
        {error, Reason}
    end.

decode(Value) ->
    decode(Value, []).


decode(Value, Opts) ->
    jsx:decode(Value, Opts).


encode(Value) ->
    jsx:encode(Value).
