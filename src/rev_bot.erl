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
        , try_read/1
        , write/1
        , register/6
        , login/3
        , gen_passwd/0
        , gen_hash/2
        , check_passwd/2
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

try_read(BotName) ->
    mnesia:dirty_read(?TABLE, BotName).

write(#rev_bot{} = Bot) ->
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Bot) end).

%%====================================================================
%% Internal functions
%%====================================================================

register(Name
         , Player
         , Desc
         , Email
         , LastIP
         , Extra
        ) when is_list(Name)   andalso Name   =/= [] andalso
               is_list(Player) andalso Player =/= [] andalso
               is_list(Desc)   andalso
               is_list(Email)  andalso Email  =/= [] andalso
               is_list(Extra)  ->
    case try_read(Name) of
        [] ->
            {PW, Hash} = gen_passwd(),
            write(#rev_bot{name=Name
                           , player=Player
                           , password=Hash
                           , descr=Desc
                           , email=Email
                           , last_ip=LastIP
                           , extra=Extra
                          }),
            {ok, PW};
        _ -> {error, already_registered}
    end.

login(Name, Password, LastIP) ->
     case try_read(Name) of
         [Bot] -> check_bot(Bot, Password, LastIP);
         []    -> {error, bad_login}
     end.

check_bot(#rev_bot{password=Hash} = Bot, Password, LastIP) ->
    case check_passwd(Hash, Password) of
        true ->
            write(Bot#rev_bot{last_ip=LastIP}),
            {ok, logged_in};
        _    ->
            {error, bad_login}
    end.


gen_passwd() ->
    TempStr = crypto:rand_bytes(16),
    Salt = base64:encode_to_string(crypto:rand_bytes(16)),
    PassWord = lists:sublist(base64:encode_to_string(TempStr), 10),
    Hash = gen_hash(Salt, PassWord),
    {PassWord, Hash}.

gen_hash(Salt, Password) ->
    Hash = crypto:md5(Salt++Password),
    {Salt, Hash}.

check_passwd({Salt, Hash}, Password) ->
    {_, Hash2} = gen_hash(Salt, Password),
    Hash =:= Hash2.
