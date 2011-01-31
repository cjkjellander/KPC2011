-module(lobby_db).

-include("../include/reversi.hrl").

%% API
-export([ create_tables/0
        , add_game/1
        , read_game/1
        , list_games/0
        , delete_game/1
        , delete_game_server/1
        , add_player/1
        , delete_player/1
        , add_ready_player/1
        , find_ready_player/0
        ]).


-define(PLAYERS, player).
-define(READY, ready).
-define(GAMES, duel).
-define(PLAYER_ATTR, [ {attributes, record_info(fields, player)}
                     , {disc_copies, [node()]}
                     ]).
-define(READY_ATTR, [ {attributes, record_info(fields, player)}
                    , {disc_copies, [node()]}
                    ]).
-define(GAMES_ATTR, [ {attributes, record_info(fields, duel)}
                    , {disc_copies, [node()]}
                    ]).

%%====================================================================
%% API
%%====================================================================
create_tables() ->
    mnesia:create_table(?PLAYERS, ?PLAYER_ATTR),
    mnesia:create_table(?READY, ?READY_ATTR),
    mnesia:create_table(?GAMES, ?GAMES_ATTR).

add_game(#duel{} = Game) ->
    Write = fun() ->
                    write_t(?GAMES, Game)
            end,
    mnesia:transaction(Write).

delete_game(Id) ->
    mnesia:transaction(fun() -> delete_t(?GAMES, Id) end).

delete_game_server(_GameServer) ->
    [].

read_game(Id) ->
    read_d(?GAMES, Id).

list_games() ->
    ok.

add_player(Pid) ->
    Write = fun() ->
                    write_t(?PLAYERS, #player{ pid = Pid })
            end,
    mnesia:transaction(Write).

delete_player(Pid) ->
    Delete = fun() ->
                     delete_t(?PLAYERS, Pid),
                     delete_t(?READY, Pid)
             end,
    mnesia:transaction(Delete).

add_ready_player(Pid) ->
    Write = fun() ->
                    [Player] = read_d(?PLAYERS, Pid),
                    write_t(?READY, Player)
            end,
    mnesia:transaction(Write).

find_ready_player() ->
    [].

%% db functions
read_d(Table, Key) ->
    mnesia:dirty_read(Table, Key).

read_t(Table, Key) ->
    mnesia:read(Table, Key).

write_t(Table, Record) ->
    mnesia:write(Table, Record, write).

delete_t(Table, Key) ->
    mnesia:delete({Table, Key}).
