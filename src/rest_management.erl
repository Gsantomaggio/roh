%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Oct 2017 11:05
%%%-------------------------------------------------------------------
-module(rest_management).
-author("gabriele").

-export([init/2, to_json_item/1]).
-export([content_types_provided/2]).
-export([status/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, status}
    ], Req, State}.


to_json_item(Item) ->
    {Node, Detail} = Item,
    json_utils:encode([{<<"node">>, Node},{<<"running_tasks">>, Detail} ]).


to_json({Replies, _BadNodes}) ->
    io_lib:format("[~s]", [json_utils:to_json_array(Replies, fun to_json_item/1)]).


status(Req, State) ->
    Body =  to_json(roh_management:call_status()),
    {Body, Req, State}.
