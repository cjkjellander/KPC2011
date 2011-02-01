-module(reversi_app).
-behaviour(application).

-export([start/0, start/2, stop/1, create_tables/0]).

start() ->
    application:start(reversi).

start(normal, _Args) ->
    %% FIXME: Make sure database tables exists and so on
    reversi_sup:start_link().

stop(_State) ->
    ok.


create_tables() ->
    [
     {rev_game_db, rev_game_db:create_table()},
     {rev_bot,     rev_bot:create_table()},
     {lobby_db,    lobby_db:create_tables()}
    ].
