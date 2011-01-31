-module(games_resource).
-export([init/1, content_types_provided/2,
         to_json/2]).

-include("rest.hrl").
-include_lib("reversi/include/reversi.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    Types = [{"application/json", to_json}],
    {Types, ReqData, State}.

to_json(ReqData, State) ->
    {ok, Games} = lobby:list_games(),
    Json =
        {struct,
         [{"games",
           {struct,
            [{"link",
              {struct,
               [{"rel", <<"self">>},
                {"href", <<"/games">>}
               ]}}
            ] ++
            [{"game",
              {struct,
               [{"id", Id},
                {"link",
                 {struct,
                  [{"rel", <<"self">>},
                   {"href", game_resource:uri(Id)}
                  ]}}
               ]}}
             || Id <- Games
            ]}}
         ]},
    {mochijson2:encode(Json), ReqData, State}.
