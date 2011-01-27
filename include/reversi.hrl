-ifndef(REVERSI_HRL).
-define(REVERSI_HRL, 1).

-record(game, {  id
               , cookie_1
               , cookie_2
               , board = {16#0000000810000000,
                          16#0000001008000000}
               , togo = 0
               , turns = 0
               , moves = []
               , points = {2, 2}
               }).

-endif.
