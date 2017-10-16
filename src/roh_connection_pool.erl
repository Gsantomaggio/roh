%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2017 16:42
%%%-------------------------------------------------------------------
-module(roh_connection_pool).
-author("gabriele").


-include_lib("../include/roh_headers.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    get_next_channel/0]).

-define(SERVER, ?MODULE).

-record(state, {current_channel = 1, connections = [], channels = []}).

%%%===================================================================
%%% API
%%%===================================================================


get_next_channel() ->
    gen_server:call(?SERVER, {get_next_channel}).




internal_setup_channels(N) when N =< 0 ->
    [];
internal_setup_channels(N) when N =< ?CHANNELS_FOR_CONNECTION ->
    lists:append(internal_setup_channels(N - 1), [{channelid, N}]).



internal_setup_connections(N) when N =< 0 ->
    [];
internal_setup_connections(N) when N =< ?CONNECTIONS_SIZE ->
    L = internal_setup_channels(?CHANNELS_FOR_CONNECTION),
    R = lists:append(internal_setup_connections(N - 1), [{N, L}]),
    R.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    L = internal_setup_connections(?CONNECTIONS_SIZE),
    roh_console_log:info("internal_setup_connections number: ~w", [L]),
    {ok, #state{connections = L}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({get_next_channel}, _From, State = #state{current_channel = CCh, channels = Ch}) when CCh < ?CHANNELS_SIZE ->
    NextChannel = lists:nth(CCh, Ch),
    roh_console_log:info("get_next_channel number: ~w", [NextChannel]),
    {reply, NextChannel, State#state{current_channel = CCh + 1}};
handle_call({get_next_channel}, _From, State = #state{channels = Ch}) ->
    NextChannel = lists:nth(1, Ch),
    roh_console_log:info("get_next_channel_0 number: ~w", [NextChannel]),
    {reply, NextChannel, State#state{current_channel = 1}};
handle_call({setup_connections, N}, _From, State = #state{connections = Cn}) ->
    roh_console_log:info("handle_call number: ~w", [N]),
    L1 = lists:append({id = N}, Cn),
    {reply, ok, State#state{connections = L1}};
handle_call(Request, _From, State) ->
    roh_console_log:info("Request Request: ~w", [Request]),
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
