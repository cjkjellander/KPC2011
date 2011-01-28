-module(game_resource).
-export([init/1, content_types_provided/2,
         resource_exists/2,
         to_html/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("../include/reversi.hrl").

init([]) -> {ok, []}.



content_types_provided(ReqData, State) ->
    Types = [{"text/html", to_html}, {"application/json", to_json}],
    {Types, ReqData, State}.

resource_exists(ReqData, State) ->
    try
        GameId = list_to_integer(wrq:path_info(foo, ReqData)),
        {ok, Status} = lobby:client_command({game, GameId, status}),
        {true, ReqData, [{game_status, Status}|State]}
    catch
        _:_ -> {false, ReqData, State}
    end.

to_html(ReqData, State) ->
    Status = proplists:get_value(game_status, State),
    {io_lib:format("~p", [Status]), ReqData, State}.

to_json(ReqData, State) ->
    {"watching game ~s ...", ReqData, State}.
