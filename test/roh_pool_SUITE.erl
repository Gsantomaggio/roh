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
-export([all/0, init_per_group/2, groups/0, end_per_group/2]).
-export([test_worker_script/1, test_max_length/1]).

all() -> [{group, roh_pool_t}].

groups() -> [{roh_pool_t, [],
    [test_worker_script, test_max_length]}].


init_per_group(roh_pool_t, Config) ->
    application:set_env(roh, python_scripts_path, proplists:get_value(data_dir, Config)),
    application:ensure_all_started(roh),
    Config.

end_per_group(roh_pool_t, _Config) ->
    application:stop(roh),
    ok.


test_worker_script(_Config) ->
    roh_pool:add_task(roh_task_util:new_task(test)),
    roh_pool:add_task(roh_task_util:new_task(test)),
    [{_, {Running, WaitingQueue}}] = roh_management:status(),
    ?assert(length(Running) == 2),
    ?assert(length(WaitingQueue) == 0),
    roh_management:stop_tasks(),
    timer:sleep(1000),
    [{_, {Running1, WaitingQueue1}}] = roh_management:status(),
    ?assert(length(Running1) == 0),
    ?assert(length(WaitingQueue1) == 0).


test_max_length(_Config) ->
    application:set_env(roh, max_waiting_queue, 1),
    roh_pool:add_task(roh_task_util:new_task(test)),
    roh_pool:add_task(roh_task_util:new_task(test)),
    [{_, {Running, WaitingQueue}}] = roh_management:status(),
    ?assert(length(Running) == 1),
    ?assert(length(WaitingQueue) == 1),
    roh_management:stop_tasks(),
    timer:sleep(1000),
    [{_, {Running1, WaitingQueue1}}] = roh_management:status(),
    ?assert(length(Running1) == 1),
    ?assert(length(WaitingQueue1) == 0),
    roh_management:stop_tasks(),
    timer:sleep(1000),
    [{_, {Running2, WaitingQueue2}}] = roh_management:status(),
    ?assert(length(Running2) == 0),
    ?assert(length(WaitingQueue2) == 0).












