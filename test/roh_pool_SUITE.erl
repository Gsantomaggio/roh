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
-include_lib("eunit/include/eunit.hrl").
-export([all/0]).
-export([test_worker_script/1]).

all() -> [test_worker_script].

start_pool() ->
    roh_app:start_pool().

test_worker_script(Config) ->
    application:set_env(roh, python_scripts_path, proplists:get_value(data_dir, Config)),
    {ok, _PID} = start_pool(),
    roh_pool:add_task(roh_task_util:new_task(test)),
    roh_pool:add_task(roh_task_util:new_task(test)),
    Status = roh_management:status(),
    [{_, List}] = Status,
    ?assert(length(List) == 2),
    roh_management:stop_tasks(),
    timer:sleep(1000),
    Status1 = roh_management:status(),
    [{_, List1}] = Status1,
    ?assert(length(List1) == 0).


