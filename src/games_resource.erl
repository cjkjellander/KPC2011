-module(games_resource).
-export([init/1, content_types_provided/2,
         to_html/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    Types = [{"text/html", to_html}, {"application/json", to_json}],
    {Types, ReqData, State}.

to_html(ReqData, State) ->
    Games = lobby:client_command({list_games}),
    {io_lib:format("~p", [Games]), ReqData, State}.

to_json(ReqData, State) ->
    %% missing hostname!
    Path = wrq:path(ReqData),
    GamesJson =
        [ {"game", {struct, [{"rel", "self"},
                             {"href", io_lib:format("~s/~s", [Path, Id]) }]}}
          || Id <- lobby:client_command({list_games})],
    Json =
        {struct,
         [{"games",
           [{"link", {struct, [{"rel", "self"},
                               {"href", Path}]}}
            |GamesJson]
          }]},
    {mochijson2:encode(Json), ReqData, State}.
