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
-export([status/0]).


status() ->
    {Replies, _BadNodes} = gen_server:multi_call([node() | nodes()], roh_pool, {status}, 5000),

    roh_console_log:out("Server status: ~n ~p", [Replies]).


