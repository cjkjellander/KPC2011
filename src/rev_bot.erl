%%%-------------------------------------------------------------------
%%% File    : rev_bot.erl
%%% Author  : Thomas Järvstrand <>
%%% Description :
%%%
%%% Created : 28 Jan 2011 by Thomas Järvstrand <>
%%%-------------------------------------------------------------------
-module(rev_bot).

-include("../include/reversi.hrl").

%% API
-export([ create_table/0
        , read/1
        , write/1
        ]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).
-define(TABLE_ATTR, [{attributes, record_info(fields,?MODULE)}]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function:
%% Description:
%%--------------------------------------------------------------------

create_table() ->
    mnesia:create_table(?TABLE, ?TABLE_ATTR).

read(BotName) ->
    {atomic, [Bot]} =
        mnesia:transaction(fun() -> mnesia:read(?TABLE, BotName) end),
    Bot.

write(#rev_bot{} = Bot) ->
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Bot) end).

%%====================================================================
%% Internal functions
%%====================================================================
