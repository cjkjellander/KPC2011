%%%-------------------------------------------------------------------
%%% File    : bot_base.erl
%%% Author  : Thomas Järvstrand <>
%%% Description : 
%%%
%%% Created : 30 Jan 2011 by Thomas Järvstrand <>
%%%-------------------------------------------------------------------
-module(bot_base).

%% API
-export([ start_link/0
        , start_link/4
        ]).

%% Callback
-export([calc_move/3]).

%%internal
-export([ init/0
        , init/4
        ]).

-include("../include/reversi.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 7676).
-define(DEFAULT_HOST, "localhost").

-define(DEFAULT_CALC_MOD, ?MODULE).

-record(state, {host = ?DEFAULT_HOST,
                port = ?DEFAULT_PORT,
                lsock,
                name = "foo",
                password = "wQXByFXlAu",
                cookie,
                calc_mod = ?DEFAULT_CALC_MOD,
                color,
                game,
                winner
               }).

%%==============================================================================
%% Move calculation
%%==============================================================================
calc_move(_Game, AvailMoves, _Color) ->
    lists:nth(random:uniform(length(AvailMoves)), AvailMoves).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    spawn_link(?MODULE, init, []),
    ok.

start_link(Host, Name, Pass, CalcMod) ->
    spawn_link(?MODULE, init, [Host, Name, Pass, CalcMod]),
    ok.

init() ->
    init(#state{}).

init(#state{} = State) ->
    %% try
    %%     R = case connect() of
    %%             {ok, LSock} ->
    %%                 connected(State#state{lsock = LSock});
    %%             Err -> Err
    %%         end,
    %%     io:format("Res:~p~n", [R])
    %% catch
    %%     _:_ = E ->
    %%         io:format("Err: ~p~n", [E])
    %% end.
    case connect() of
        {ok, LSock} ->
            connected(State#state{lsock = LSock});
        Err -> Err
    end.

init(Host, Name, Pass, CalcMod) ->
    State = #state{host = Host,
                   name = Name,
                   password = Pass,
                   calc_mod = CalcMod},
    init(State).

connected(State) ->
    case login(State) of
        {ok, welcome} -> logged_in(State);
        Err           -> Err
    end.

%% In the game lobby
logged_in(#state{game = undefined, lsock = LSock} = State) ->
    case new_game(LSock) of
        {lets_play, Color, GameId, Cookie} ->
            game_created(State#state{color = Color,
                                     game = #game{id = GameId},
                                     cookie = Cookie});
        Err  -> Err
    end.

%% Opponent found
game_created(#state{lsock = LSock, cookie = Cookie, color = Color} = State) ->
    case game_log_in(LSock, Cookie, Color) of
        {please_wait, Game} -> awaiting_turn(State#state{game = Game});
        {your_move, Game}   -> making_move(State#state{game = Game});
        Err                 -> Err
    end.

%% Waiting for my turn
awaiting_turn(State) ->
    case recv() of
        {ok, {your_move, Game}} -> making_move(State#state{game = Game});
        {ok, {game_over, UpdatedGame, Winner}} ->
            game_over(State#state{game = UpdatedGame, winner = Winner});
        Err -> Err
    end.

making_move(#state{lsock = LSock, cookie = Cookie, game = Game, color = Color, calc_mod = Mod} = State) ->
    io:format("~p(~p) making move~n", [State#state.name, self()]),
    Moves = reversi:check_avail(Game, Color),
    {X, Y, _GS} = Mod:calc_move(Game, Moves, Color),
    case make_move(LSock, Cookie, X, Y, Color) of
        {please_wait, UpdatedGame}        -> awaiting_turn(State#state{game = UpdatedGame});
        {game_over, UpdatedGame, Winner} ->
            game_over(State#state{game = UpdatedGame, winner = Winner});
        Err                               -> Err
    end.

game_over(#state{winner = Me, color = Me} = State) ->
    io:format("You win!!!~n"),
    finish(State);
game_over(State) ->
    io:format("You loose!!!~n"),
    finish(State).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

connect() ->
    connect(?DEFAULT_HOST, ?DEFAULT_PORT).
connect(Host, Port) ->
    gen_tcp:connect(Host, Port, [binary,{packet, 0}]).

disconnect(LSock) ->
    gen_tcp:close(LSock).

login(#state{lsock = LSock, name = Name, password = Password}) ->
    do_rpc(LSock, {login, Name, Password}).

new_game(LSock) ->
    case do_rpc(LSock, {i_want_to_play}) of
        {ok, {lets_play, _Color, _GameId, _Cookie} = Res} -> Res;
        {ok,  waiting_for_challenge} -> wait_for_game()
    end.

wait_for_game() ->
    case recv() of
        {ok, {lets_play, _Color, _GameId, _Cookie} = Res} -> Res;
        Err                                      -> Err
    end.

game_log_in(LSock, Cookie, Color) ->
    case do_rpc(LSock, {login, Cookie, Color}) of
        {ok, wait_for_other_guy} ->
            case recv() of
                {ok, {please_wait, _Game} = Res} -> Res;
                {ok, {your_move, _Game} = Res}   -> Res;
                Err                               -> Err
            end;
        {ok, {please_wait, _Game} = Res} -> Res;
        {ok, {your_move, _Game} = Res}   -> Res;
        Err                              -> Err
    end.

make_move(LSock, Cookie, X, Y, Color) ->
    case do_rpc(LSock, {move, Cookie, Color, X, Y}) of
        {ok, {please_wait, _Game} = Res}        -> Res;
        {ok, {game_over, _Game, _Winner} = Res} -> Res;
        Err                                     -> Err
    end.

finish(#state{lsock = LSock}) ->
    case do_rpc(LSock, {logout}) of
        good_bye -> disconnect(LSock);
        Err      -> Err
    end.

term_to_string(Term) ->
    lists:flatten(io_lib:format("~p.", [Term])).

do_rpc(LSock, Msg) ->
    gen_tcp:send(LSock, term_to_string(Msg)),
    recv().

recv() ->
    receive
        {tcp, _Socket, Packet} ->
            %% io:format("~p received ~p~n",
            %%           [self(), tcp_parse:parse_data(binary_to_list(Packet) ++ ".")]),
            tcp_parse:parse_data(binary_to_list(Packet) ++ ".");
        Err -> Err
    end.
