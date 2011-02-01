-module(gameboard_resource).
-export([uri/1, init/1,
         content_types_provided/2,
         resource_exists/2,
         to_json/2]).

-include("rest.hrl").
-include_lib("reversi/include/reversi.hrl").
-include_lib("webmachine/include/webmachine.hrl").

uri(GameId) when is_integer(GameId) ->
    list_to_binary(io_lib:format("~s/games/~w/board", [?HOSTPORT, GameId])).

init([]) -> {ok, []}.

content_types_provided(ReqData, State) ->
    Types = [{"application/json", to_json}],
    {Types, ReqData, State}.

resource_exists(ReqData, State) ->
    try
        GameId = list_to_integer(wrq:path_info(game_id, ReqData)),
        {ok, Game} = lobby:get_game(GameId),
        {true, ReqData, [{game, Game}|State]}
    catch
        _:_ -> {false, ReqData, State}
    end.

to_json(ReqData, State) ->
    Game = proplists:get_value(game, State),
    GameId = Game#game.id,
    {Rows,_} = lists:mapfoldl(fun([A,B,C,D,E,F,G,H], Row) ->
                                      Json = {"row",
                                              {struct,
                                               [{"number", Row},
                                                {"A", A},
                                                {"B", B},
                                                {"C", C},
                                                {"D", D},
                                                {"E", E},
                                                {"F", F},
                                                {"G", G},
                                                {"H", H}
                                               ]}},
                                      {Json, Row+1}
                              end, 1, reversi:game2lists(Game)),

    Json = {struct,
            [{"board",
              {struct,
               [{"game",
                 {struct,
                  [{"id", GameId},
                   {"link", {struct,
                             [{"rel", <<"self">>},
                              {"href", game_resource:uri(GameId)}
                             ]}}
                  ]}}
               ] ++ Rows}}
            ]},
    {mochijson2:encode(Json), ReqData, State}.
