-module(bot_resource).
-export([uri/1, games_uri/1,
         init/1,
         content_types_provided/2,
         resource_exists/2,
         to_json/2]).

-include("rest.hrl").
-include_lib("reversi/include/reversi.hrl").
-include_lib("webmachine/include/webmachine.hrl").

uri(BotName) when is_list(BotName) ->
    list_to_binary(io_lib:format("~s/bots/~s", [?HOSTPORT, BotName])).
games_uri(BotName) when is_list(BotName) ->
    list_to_binary(io_lib:format("~s/bots/~s/games", [?HOSTPORT, BotName])).

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    Types = [{"application/json", to_json}],
    {Types, ReqData, State}.

resource_exists(ReqData, State) ->
    try
        BotName = wrq:path_info(bot_name, ReqData),
        {ok, Bot} = lobby:get_bot(BotName),
        {true, ReqData, [{bot, Bot}|State]}
    catch
        _:_ -> {false, ReqData, State}
    end.

to_json(ReqData, State) ->
    Bot = proplists:get_value(bot, State),
    Name = Bot#rev_bot.name,
    Description = list_to_binary(Bot#rev_bot.descr),
    Rank = Bot#rev_bot.rank,
    Player = Bot#rev_bot.player,
    Json =
        {struct,
         [{"bot",
           {struct,
            [{"name", list_to_binary(Name)},
             {"link",
              {struct,
               [{"rel", <<"self">>},
                {"href", uri(Name)}
               ]}},
             {"description", Description},
             {"rank", Rank},
             {"player",
              {struct,
               [{"name", list_to_binary(Player)}
               ]}},
             {"games",
              {struct,
               [{"link",
                 {struct,
                  [{"rel", <<"self">>},
                   {"href", games_uri(Name)}
                  ]}}
               ]}}
            ]}}
         ]},
    {mochijson2:encode(Json), ReqData, State}.
