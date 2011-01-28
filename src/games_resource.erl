-module(games_resource).
-export([init/1, content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    Types = [{"application/json", to_json}],
    {Types, ReqData, State}.

to_json(ReqData, State) ->
    %% missing hostname!
    Path = wrq:path(ReqData),
    Games = lobby:client_command({list_games}),
    Json =
        {struct,
         [{"games",
           {struct,
            [{"link", {struct,
                       [{"rel", <<"self">>},
                        {"href", list_to_binary(Path)}
                       ]}}
            ] ++
            [{"game", {struct,
                       [{"rel", <<"self">>},
                        {"href", list_to_binary(
                                   io_lib:format("~s/~w", [Path, Id])) }
                       ]}}
             || Id <- Games
            ]}
          }]},
    {mochijson2:encode(Json), ReqData, State}.
