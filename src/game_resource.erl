-module(game_resource).
-export([init/1, content_types_provided/2,
         resource_exists/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("../include/reversi.hrl").

init([]) -> {ok, []}.

content_types_provided(ReqData, State) ->
    Types = [{"application/json", to_json}],
    {Types, ReqData, State}.

resource_exists(ReqData, State) ->
    try
        GameId = list_to_integer(wrq:path_info(foo, ReqData)),
        {ok, Status} = lobby:client_command({game, GameId, status}),
        {true, ReqData, [{game_status, Status}|State]}
    catch
        _:_ -> {false, ReqData, State}
    end.

to_json(ReqData, State) ->
    Status = proplists:get_value(game_status, State),
    Board = reversi:game2lists(Status),
    Json = {struct,
            ["game",
             {struct,
              [{"link", {struct,
                         [{"rel","self"},
                          {"href", list_to_binary(wrq:path(ReqData))}
                         ]}},
               {"id", Status#game.id},
               {"player", {struct,
                           ["link", {struct,
                                     [{"rel", "self"},
                                      {"href", "/players/ONE"}
                                     ]}
                           ]}},
               {"player", {struct,
                           ["link", {struct,
                                     [{"rel", "self"},
                                      {"href", "/players/TWO"}
                                     ]}
                           ]}},
               {"start_time", Status#game.start_time},
               {"board", Board}
              ]}
            ]},
    {mochijson2:encode(Json), ReqData, State}.
