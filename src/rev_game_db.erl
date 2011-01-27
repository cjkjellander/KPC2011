%%%-------------------------------------------------------------------
%%% File    : reversi_game_db.erl
%%% Author  : Thomas Järvstrand <>
%%% Description : 
%%%
%%% Created : 27 Jan 2011 by Thomas Järvstrand <>
%%%-------------------------------------------------------------------
-module(reversi_game_db).

-behaviour(gen_server).

-include("../include/reversi.hrl").

%% API
-export([ create_table/0
        , start_link/0
        , stop/0
        , new_game/0
        , update_game/1
        , get_game/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {next_id}).

-define(SERVER, ?MODULE).
-define(TABLE, game).
-define(TABLE_ATTR, [{attributes, record_info(fields,game)}]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
create_table() ->
    mnesia:create_table(?TABLE, ?TABLE_ATTR).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_game() ->
    gen_server:call(?SERVER, new_game).

update_game(#game{} = Game) ->
    gen_server:call(?SERVER, {update_game, Game}).

get_game(Id) when is_integer(Id) ->
    gen_server:call(?SERVER, {get_game, Id}).

stop() ->
    gen_server:call(?SERVER, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    NextId = case mnesia:dirty_all_keys(?TABLE) of
                 []  -> 0;
                 Keys-> lists:max(Keys) + 1
             end,
    {ok, #state{next_id = NextId}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(new_game, _From, #state{next_id = NextId} = State) ->
    Game = #game{id = NextId},
    write(Game),
    {reply, {ok, Game}, State#state{next_id = NextId + 1}};
handle_call({update_game, Game}, _From, State) ->
    write(Game),
    {reply, {ok, Game}, State };
handle_call({get_game, Id}, _From, State) ->
    {reply, {ok, read(Id)}, State};
handle_call(stop, _From, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

write(#game{} = Game) ->
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Game) end).

read(#game{id = Id}) ->
    read(Id);
read(Id) when is_integer(Id)->
    {atomic, [Game]} = mnesia:transaction(fun() -> mnesia:read(?TABLE, Id) end),
    Game.
