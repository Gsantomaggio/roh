%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2017 15:21
%%%-------------------------------------------------------------------
-module(roh_console_log).
-author("gabriele").
-behavior(roh_log_interface).

%% API
-export([info/2, debug/2, warning/2, error/2, level/1]).


write_message(Msg, Params) ->
    io:format(Msg, Params).

info(Msg, Params) ->
    write_message("[INFO] " ++ Msg, Params),
    ok.
debug(Msg, Params) ->
    write_message("[DEBUG] " ++ Msg, Params),
    ok.

warning(Msg, Params) ->
    write_message("[WARNING] " ++ Msg, Params),
    ok.

error(Msg, Params) ->
    write_message("[ERROR] " ++ Msg, Params),
    ok.


level(_Msg) -> ok.
