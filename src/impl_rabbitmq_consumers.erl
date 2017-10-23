%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Oct 2017 11:11
%%%-------------------------------------------------------------------
-module(impl_rabbitmq_consumers).
-author("gabriele").

-include_lib("../include/roh_headers.hrl").


%% API
-export([lc/0, add_local_consumer/1, add_cluster_consumer/1]).


lc() ->
    add_local_consumer(5672),
    add_local_consumer(5673).

getTask(Port) ->
    UUID = erlang:phash2({rand:uniform(500), now()}),
    Task = #task{id = UUID, module_start = consumer, function_start = start, parameters_start = [localhost, Port],
        module_stop = consumer, function_stop = stop, parameters_stop = []},
    Task.

add_local_consumer(Port) ->
    roh_pool:add_task(getTask(Port)).

add_cluster_consumer(Port) ->
    gen_server:multi_call([node() | nodes()], roh_pool, {add_task, getTask(Port)}, 5000).




