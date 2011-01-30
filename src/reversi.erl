-module(reversi).
-export([new_game/1
         , draw_board/1
         , piece/3
         , set_piece/4
         , move/4
         , score/1
         , check_avail/2
         , move_check/1
         , rand_pick/1
         , winner/1
         , pname/1
         , rand_play/1
         , game2lists/1
         ]).

-include("../include/reversi.hrl").

new_game(N) ->
    {ok, #game{id=N}}.

draw_board(#game{} = G) ->
    io:format(" +--------+~n"),
    draw_lines(G, 0),
    io:format(" +--------+~n"
              "  ABCDEFGH~n");
draw_board(Board) ->
    draw_board(#game{board = Board}).

draw_lines(_, 8) -> ok;
draw_lines(#game{} = G, Y) ->
    io:format("~c|~s~n", [$1 + Y, draw_line(G, 0, Y)]),
    draw_lines(G, Y+1).

draw_line(_, 8, _) -> "|";
draw_line(#game{} = G, X, Y) ->
    [draw_point(G, X, Y) | draw_line(G, X+1, Y)].

draw_point(#game{board = Board}, X, Y) ->
    case piece(Board, X, Y) of
        ?B -> $B;
        ?W -> $W;
        ?E -> $.;
        _  -> erlang:error({board_inconsistent, Board})
    end.

piece(_, X, Y) when X>7; X<0; Y>7; Y<0 -> ?E;
piece({Black, White}, X, Y) ->
    case {Black band bit_set(X,Y) > 0,
          White band bit_set(X,Y) > 0} of
        {false, false} -> ?E;
        {true,  false} -> ?B;
        {false, true}  -> ?W;
        {true,  true}  -> incosistent_board
    end.

set_piece(_, X, Y, _) when X>7; X<0; Y>7; Y<0 ->
    {error, {outside_board, X, Y}};
set_piece({Black, White}, X, Y, ?B) ->
    {Black bor bit_set(X, Y),
     White band bnot bit_set(X, Y)};
set_piece({Black, White}, X, Y, ?W) ->
    {Black band bnot bit_set(X, Y),
     White bor bit_set(X, Y)}.


bit_set(X, Y) ->
    1 bsl (8*Y + X).

move(#game{} = G, X, Y, _) when X>7; X<0; Y>7; Y<0 ->
    {error, {illegal_move, G}};
move(#game{board = B} = G, X, Y, Who) when Who =:= ?B orelse
                                           Who =:= ?W ->
    case piece(B, X, Y) of
        ?E -> maybe_move(G, X, Y, Who);
        _  -> {error, {illegal_move, G}}
    end;
move(#game{} = G,_,_,_) ->
    {error, {illegal_move, G}}.

maybe_move(#game{board = B} = G, X, Y, Who) ->
    F = fun({Xd, Yd}, BA) ->
                find_start(BA, X+Xd, Y+Yd, Who, Xd, Yd,
                           set_piece(BA, X, Y, Who))
        end,
    B1 = lists:foldl(F, B, directions()),
    case B =/= B1 of
        true -> do_move(G, X, Y, Who, B1);
        _    -> {error, {illegal_move, G}}
    end.

do_move(#game{board = OldBoard, turns = T, moves = M} = G,
        X, Y, Who, NewBoard) ->
    Score = score(NewBoard),
    {ok, G#game{board = NewBoard
                , togo = other_guy(Who)
                , moves = [{T+1, Who, X, Y, OldBoard}|M]
                , turns = T+1
                , points = Score}}.

find_start(Board, X, Y, Who, Xd, Yd, AccB) ->
    case piece(Board, X, Y) =/= other_guy(Who) of
        true  -> Board;
        false -> find_flip(Board, X, Y, Who, Xd, Yd, AccB)
    end.

find_flip(Board, X, Y, Who, Xd, Yd, AccB) ->
    Other = other_guy(Who),
    case piece(Board, X, Y) of
        ?E    -> Board;
        Who   -> set_piece(AccB, X, Y, Who);
        Other -> find_flip(Board, X+Xd, Y+Yd, Who, Xd, Yd,
                           set_piece(AccB, X, Y, Who))
    end.

other_guy(?B) -> ?W;
other_guy(?W) -> ?B.

directions() ->
    [{   1,  0}
     , { 1,  1}
     , { 0,  1}
     , {-1,  1}
     , {-1,  0}
     , {-1, -1}
     , { 0, -1}
     , { 1, -1}
    ].

score({Black, White}) ->
    case consistent({Black, White}) of
        true ->
            {count_ones(Black), count_ones(White)};
        _ -> erlang:error({board_incosistent, {Black, White}})
    end.

consistent({Black, White}) ->
    (Black band White) =:= 0.

count_ones(I) ->
    count_ones(I, 0).

count_ones(0, Ones) -> Ones;
count_ones(I, Ones) ->
    case I band 1 of
        1 -> count_ones(I bsr 1, Ones+1);
        0 -> count_ones(I bsr 1, Ones)
    end.


check_avail(#game{} = G, Who) ->
    [{X, Y, GN} || X <- [0,1,2,3,4,5,6,7],
                   Y <- [0,1,2,3,4,5,6,7],
                   case move(G, X, Y, Who) of
                       {ok, GN} -> true;
                       GN -> false
                   end];
check_avail(Board, Who) ->
    check_avail(#game{board = Board}, Who).

game2lists(#game{board=Board}) ->
    [ [piece(Board,X,Y) || X <- [0,1,2,3,4,5,6,7]]
      || Y <- [0,1,2,3,4,5,6,7]].

move_check(#game{togo = Who} = G) ->
    O = other_guy(Who),
    case check_avail(G, Who) of
        [] -> case check_avail(G, O) of
                  [] -> {done, G, winner(G)};
                  M1 -> {switch, G#game{togo=O}, M1}
              end;
        M2 -> {go, G, M2}
    end.

winner(#game{board = B}) ->
    {Bl, Wh} = score(B),
    case X=Bl - Wh of
        X when X>0 -> {?B, Bl, Wh};
        X when X<0 -> {?W, Bl, Wh};
        _   -> {?E, Bl, Wh}
    end.

rand_play(N) ->
    {ok, G} = new_game(N),
    rand_loop(G).

rand_loop(#game{togo=Who} = G) ->
    draw_board(G),
    case move_check(G) of
        {done, G, {Win, B, W}} ->
            io:format("~n~s wins! Black: ~p, White: ~p~n",
                      [pname(Win), B, W]),
            G;
        {go, _, M} ->
            {X,Y,GN} = rand_pick(M),
            show_move(G, X, Y, Who),
            rand_loop(GN);
        {switch, _, M} ->
            {X,Y,GN} = rand_pick(M),
            show_move(G, X, Y, other_guy(Who)),
            rand_loop(GN)
    end.

rand_pick(L) ->
    Len = length(L),
    lists:nth(random:uniform(Len), L).

show_move(G, X, Y, Who) ->
    io:format("B: ~p W: ~p~n~n~s plays: ~c~c~n~n",
              [element(?B+1, G#game.points),
               element(?W+1, G#game.points),
               pname(Who), $A+X, $1+Y
              ]).


pname(?B) -> "Black";
pname(?W) -> "White";
pname(?E) -> "Nobody".
