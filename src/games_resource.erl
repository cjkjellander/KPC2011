-module(games_resource).
-export([init/1, content_types_provided/2,
         to_html/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    Types = [{"text/html", to_html}, {"application/json", to_json}],
    {Types, ReqData, State}.

to_html(ReqData, State) ->
    {"html list of games resources ...", ReqData, State}.

to_json(ReqData, State) ->
    {"json list of games resources ...", ReqData, State}.
