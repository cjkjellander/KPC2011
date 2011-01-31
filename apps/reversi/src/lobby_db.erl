-module(lobby_db).

-include_lib("stdlib/include/qlc.hrl").

-include("../include/reversi.hrl").

%% API
-export([ create_tables/0
        , create_player_table/0
        , create_ready_table/0
        , create_duel_table/0
        , add_game/1
        , read_game/1
        , list_games/0
        , delete_game/1
        , delete_game_server/1
        , add_player/2
        , read_player/1
        , delete_player/1
        , add_ready_player/1
        , find_ready_player/0
        ]).


-define(PLAYERS, player).
-define(READY, ready).
-define(GAMES, duel).
-define(PLAYER_ATTR, [ {attributes, record_info(fields, player)}
                     , {record_name, player}
                     , {disc_copies, [node()]}
                     ]).
-define(READY_ATTR, [ {attributes, record_info(fields, player)}
                    , {record_name, player}
                    , {disc_copies, [node()]}
                    ]).
-define(GAMES_ATTR, [ {attributes, record_info(fields, duel)}
                    , {disc_copies, [node()]}
                    ]).

%%====================================================================
%% API
%%====================================================================
create_tables() ->
    create_player_table(),
    create_ready_table(),
    create_duel_table().

create_player_table() ->
    {atomic, ok} = mnesia:create_table(?PLAYERS, ?PLAYER_ATTR).
create_ready_table() ->
    {atomic, ok} = mnesia:create_table(?READY, ?READY_ATTR).
create_duel_table() ->
    {atomic, ok} = mnesia:create_table(?GAMES, ?GAMES_ATTR).

add_game(#duel{} = Game) ->
    Write = fun() ->
                    write_t(?GAMES, Game)
            end,
    transaction(Write).

delete_game(Id) ->
    transaction(fun() -> delete_t(?GAMES, Id) end).

delete_game_server(GameServer) ->
    MaybeDelete = fun(#duel{ game_id = Id} = Duel, Count) ->
                          case Duel#duel.game_server of
                              GameServer ->
                                  delete_game(Id),
                                  Count + 1;
                              _ ->
                                  Count
                          end
                  end,
    Drop = fun() ->
                   mnesia:foldl(MaybeDelete, 0, ?GAMES)
           end,
    transaction(Drop).

read_game(Id) ->
    read_d(?GAMES, Id).

list_games() ->
    Games = fun() ->
                    Q = qlc:q([Game || Game <- mnesia:table(?GAMES)]),
                    qlc:e(Q)
            end,
    transaction(Games).

add_player(Pid, Name) ->
    Write = fun() ->
                    write_t(?PLAYERS, #player{ pid = Pid, name = Name })
            end,
    transaction(Write).

read_player(Id) ->
    read_d(?PLAYERS, Id).

delete_player(Pid) ->
    Delete = fun() ->
                     delete_t(?PLAYERS, Pid),
                     delete_t(?READY, Pid)
             end,
    transaction(Delete).

add_ready_player(Pid) ->
    Write = fun() ->
                    [Player] = read_d(?PLAYERS, Pid),
                    write_t(?READY, Player)
            end,
    transaction(Write).

find_ready_player() ->
    Find = fun() ->
                   case mnesia:first(?READY) of
                       '$end_of_table' ->
                           [];
                       Key ->
                           Player = read_t(?READY, Key),
                           delete_t(?READY, Key),
                           Player
                   end
           end,
    transaction(Find).

%% db functions
read_d(Table, Key) ->
    mnesia:dirty_read(Table, Key).

read_t(Table, Key) ->
    mnesia:read(Table, Key).

write_t(Table, Record) ->
    mnesia:write(Table, Record, write).

delete_t(Table, Key) ->
    mnesia:delete({Table, Key}).

transaction(Fun) ->
    {atomic, Result} = mnesia:transaction(Fun),
    Result.
