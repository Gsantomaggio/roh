%%%-------------------------------------------------------------------
%%% @author GaS
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jan 2018 12:43
%%%-------------------------------------------------------------------
-module(roh_task_util).
-include_lib("../include/roh_headers.hrl").
-author("GaS").

%% API
-export([new_task/1]).

new_task(Module) ->
    UUID = erlang:phash2({rand:uniform(500), erlang:timestamp()}),
    Task = #task{id = UUID, module_start = Module, function_start = start, parameters_start = [],
        module_stop = Module, function_stop = stop, parameters_stop = []},
    Task.

