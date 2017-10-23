%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2017 08:43
%%%-------------------------------------------------------------------
-author("gabriele").

-record(task, {id,
    module_start, function_start, parameters_start = [],
    module_stop, function_stop, parameters_stop = [],
    time_start, status}).

-define(SHUTDOWN_TIMEOUT, 5000).
-define(WORKER(I), {I, {I, start_link, []}, temporary, ?SHUTDOWN_TIMEOUT, worker, [I]}).

-define(CONNECTIONS_SIZE, 10).
-define(CHANNELS_FOR_CONNECTION, 2).
-define(CHANNELS_SIZE, ?CONNECTIONS_SIZE * ?CHANNELS_FOR_CONNECTION).

