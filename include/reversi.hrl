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
               , time_left = {{0, 300, 0},
                              {0, 300, 0}}
               }).

-record(rev_bot, {  name = []
                  , player = []
                  , password = []
                  , descr = []
                  , email = []
                  , rank = 1000
                  , last_ip = []
                  , extra = []
             }).

-record(player, { pid }).

-record(duel, { game_id
              , game_server
              , game_data
              , black
              , white
              }).

-define(B, 0).
-define(W, 1).
-define(E, -1).

-endif.
