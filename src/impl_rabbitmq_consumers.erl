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
-export([a/0, add_consumer/1]).


a() ->
    add_consumer(5672),
    add_consumer(5673).

add_consumer(Port) ->
    UUID = erlang:phash2({rand:uniform(500), now()}),
    Task = #task{id = UUID, module_start = consumer, function_start = start, parameters_start = [localhost, Port],
        module_stop = consumer, function_stop = stop, parameters_stop = []},
    R = roh_pool:add_task(Task),
    R.

