%%%-------------------------------------------------------------------
%%% File    : bot.erl
%%% Author  : Thomas Järvstrand <>
%%% Description : 
%%%
%%% Created : 28 Jan 2011 by Thomas Järvstrand <>
%%%-------------------------------------------------------------------
-module(bot).

-include("../include/reversi.hrl").

%% API
-export([ start/1
         ,start_link/1]).

-export([init/1]).

-define(PORT, 7676).

-record(state, {port = ?PORT, lsock, game}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Server) ->
    register(?MODULE, spawn(?MODULE, init, [Server])).

start_link(Server) ->
    register(?MODULE, spawn_link(?MODULE, init, [Server])).

init(Server) ->
    LSock = connect(Server),
    reg(LSock),
    login(LSock),
    Game = new_game(LSock),
    play(#state{lsock = LSock, game = Game}).

play(#state{lsock = LSock, game = Game} = State) ->
    AvailMoves = get_moves(LSock, Game),
    Move = best_move(AvailMoves, Game),
    case make_move(LSock, Move) of
        #game{} = Game ->
            play(State#state{game = Game});
        {game_over, Res} ->
            io:format("Game over, you ~p~n", [Res])
    end.

connect(Server) ->
    gen_tcp:connect(Server, ?PORT, []).

reg(LSock) ->
    Name = "foo",
    Player = "foo",
    Descr = "bar",
    Email = "foo@bar.baz",
    do_rpc(LSock, {register, Name, Player, Descr, Email}).

login(LSock) ->
    welcome = do_rpc(LSock, {login, "foo", "bar"}),
    ok.

new_game(LSock) ->
    get_ready_for_some_action = do_rpc(LSock, {i_want_to_play}),
    {ok,{game,0,undefined,undefined,
         {34628173824,68853694464},
         0,0,[],
         {2,2},
         undefined,undefined,
         {{0,300,0},{0,300,0}}}}.

get_moves(LSock, GameId) ->
    do_rpc(LSock, {get_moves, GameId}).

best_move(AvailMoves, _Game) ->
    lists:nth(rand:uniform(length(AvailMoves))).

make_move(LSock, Move) ->
    do_rpc(LSock, {make_move, Move}).

term_to_string(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

do_rpc(LSock, Msg) ->
    gen_tcp:send(LSock, term_to_string(Msg)),
    case gen_tcp:recv(LSock, 0) of
        {ok, Packet} -> tcp_parse:parse_data(Packet);
        Err -> Err
    end.
