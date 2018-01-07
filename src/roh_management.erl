%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2017 10:19
%%%-------------------------------------------------------------------
-module(roh_management).
-author("gabriele").

%% API
-export([status/0, stop_tasks/0, stop_task/1, call_status/0]).


call_status() ->
     gen_server:multi_call([node() | nodes()], roh_pool, {status}, 5000).

status() ->
    {Replies, _BadNodes} = call_status(),
    Replies.


stop_tasks() ->
    {Replies, _BadNodes} = gen_server:multi_call([node() | nodes()], roh_pool, {stop_all_tasks}, 5000),
    roh_console_log:out("Tasks stopped: ~n ~p", [Replies]).


stop_task(ID) ->
    {Replies, _BadNodes} = gen_server:multi_call([node() | nodes()], roh_pool, {stop_task, ID}, 5000),
    roh_console_log:out("Task stopped: ~n ~p", [Replies]).


