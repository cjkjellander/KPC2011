-module(bots_resource).
-export([init/1,
         content_types_provided/2,
         to_json/2]).

-include("rest.hrl").
-include_lib("reversi/include/reversi.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    Types = [{"application/json", to_json}],
    {Types, ReqData, State}.

to_json(ReqData, State) ->
    {ok, Bots} = lobby:list_bots(),
    Json =
        {struct,
         [{"bots",
           {struct,
            [{"link",
              {struct,
               [{"rel", <<"self">>},
                {"href", <<"/bots">>}
               ]}}
            ] ++
            [{"bot",
              {struct,
               [{"name", list_to_binary(Name)},
                {"link",
                 {struct,
                  [{"rel", <<"self">>},
                   {"href", bot_resource:uri(Name)}
                  ]}}
               ]}}
             || Name <- Bots
            ]}}
         ]},
    {mochijson2:encode(Json), ReqData, State}.
