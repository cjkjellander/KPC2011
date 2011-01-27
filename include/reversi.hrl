-ifndef(REVERSI_HRL).
-define(REVERSI_HRL, 1).

-record(game, {  id
               , player_b
               , player_w
               , board = {16#0000000810000000,
                          16#0000001008000000}
               , togo = 0
               , turns = 0
               , moves = []
               , points = {2, 2}
               , start_time
               , end_time
               }).

-endif.
