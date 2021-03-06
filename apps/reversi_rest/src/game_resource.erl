-module(game_resource).
-export([uri/1, init/1,
         content_types_provided/2,
         resource_exists/2,
         to_json/2]).

-include("rest.hrl").
-include_lib("reversi/include/reversi.hrl").
-include_lib("webmachine/include/webmachine.hrl").

uri(GameId) when is_integer(GameId) ->
    list_to_binary(io_lib:format("~s/games/~w", [?HOSTPORT, GameId])).

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
    StartTime = unix_timestamp(Game#game.start_time),
    EndTime = unix_timestamp(Game#game.end_time),
    Turn = color(Game#game.togo),
    BotBlack = Game#game.player_b,
    BotWhite = Game#game.player_w,

    Json = {struct,
            [{"game",
              {struct,
               [{"id", GameId},
                {"link",
                 {struct,
                  [{"rel", <<"self">>},
                   {"href", uri(GameId)}
                  ]}},
                {"start_time", StartTime},
                {"end_time", EndTime},
                {"turn", Turn},
                {"bot", {struct,
                         [{"color", color(?B)},
                          {"name", list_to_binary(BotBlack)},
                          {"link", {struct,
                                    [{"rel", <<"self">>},
                                     {"href", bot_resource:uri(BotBlack)}
                                    ]}}
                         ]}},
                {"bot", {struct,
                         [{"color", color(?W)},
                          {"name", list_to_binary(BotWhite)},
                          {"link", {struct,
                                    [{"rel", <<"self">>},
                                     {"href", bot_resource:uri(BotWhite)}
                                    ]}}
                         ]}},
                {"board", {struct,
                           [{"link", {struct,
                                      [{"rel", <<"self">>},
                                       {"href", gameboard_resource:uri(GameId)}
                                      ]}}
                           ]}}
               ]}}
            ]},
    {mochijson2:encode(Json), ReqData, State}.

color(?B) ->
    <<"black">>;
color(?W) ->
    <<"white">>.

unix_timestamp(undefined) ->
  %% start_time should never be undefined, but it would be normal for an ongoing
  %% game to have end_time = undefined, and thats ok
    <<"">>;
unix_timestamp({MegaSecs, Secs, _MicroSecs}) ->
    UnixTime = MegaSecs * 1000000 + Secs,
    list_to_binary(integer_to_list(UnixTime)).
