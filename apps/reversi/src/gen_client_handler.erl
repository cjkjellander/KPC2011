-module(gen_client_handler).

-export([behaviour_info/1]).


behaviour_info(callbacks) ->
    [
     {your_move, 1},
     {nothing_to_do, 1},
     {game_over, 2}
    ].
