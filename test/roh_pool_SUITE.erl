%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2017 22:31
%%%-------------------------------------------------------------------
-module(roh_pool_SUITE).
-author("gabriele").

-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test_worker_script/1]).

all() -> [test_worker_script].

init() ->
    roh_app:start_pool().

test_worker_script(_Config) ->
    {ok, _PID} = init(),
    roh_pool:add_task(roh_task_util:new_task(test)),
    ct:log("ciao"),

    1 = 1.

