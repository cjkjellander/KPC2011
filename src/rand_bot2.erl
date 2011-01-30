-module(rand_bot2).

-export([
         start/4
        ]).

start(Host, Port, Name, Passwd) ->
    case gen_tcp:connect(Host, Port, [binary,{packet, 0}]) of
	{ok, Sock} ->
            login(Sock, Name, Passwd),
	    gen_tcp:close(Sock);
	_ ->
	    error
    end.

login(Sock, Name, Passwd) ->
    catch wait_reply(1000),
    Login = mk_login(Name, Passwd),
    Reply = send_cmd(Sock, Login),
    case parse_data(Reply) of
        {ok, welcome} -> ready(Sock, Name, Passwd);
        _ -> ok
    end.

ready(Sock, Name, Passwd) ->
    Ready = mk_ready(),
    Reply = send_cmd(Sock, Ready),
    case parse_data(Reply) of
        {ok, waiting_for_challenge} ->
            wait_for_chal(Sock, Name, Passwd);
        {ok, {lets_play, Who, Game}} ->
                start_game(Sock, Name, Passwd, Who, Game);
        _ -> ok
    end.

wait_for_chal(Sock, Name, Passwd) ->
    {value, {_,_,Reply}} = wait_long_reply(Sock),
    case parse_data(Reply) of
        {ok, {lets_play, Who, Game}} ->
            start_game(Sock, Name, Passwd, Who, Game);
        _ -> ok
    end.

start_game(Sock, Name, Passwd, Who, Game) ->
    Ready = mk_start(Who),
    Reply = send_cmd(Sock, Ready),
    case parse_data(Reply) of
        _ -> ok
    end.


send_cmd(Sock, Cmd) ->
    io:format("~s~n", [Cmd]),
    gen_tcp:send(Sock, Cmd),
    {value, {_,_,Reply}} = wait_reply(Sock),
    io:format("~s~n", [binary_to_list(Reply)]),
    Reply.

wait_reply(_Timeout) ->
    receive
	Reply ->
	    {value, Reply}
    after 1000 ->
	    timeout
    end.

wait_long_reply(_Timeout) ->
    receive
	Reply ->
	    {value, Reply}
    after 100000 ->
	    timeout
    end.

mk_login(Name, Passwd) ->
    io_lib:format("{login,\"~s\",\"~s\"}.", [Name, Passwd]).

mk_ready() ->
    "{i_want_to_play}.".

mk_start(Who) ->
    io_lib:format("{login,\"~s\"}.", [Who]).



parse_data("\n") ->
    [];
parse_data(RawData) ->
    parse_data2(binary_to_list(RawData) ++ ".").

parse_data2(RawData) ->
    try
        {ok, Tokens, _} = erl_scan:string(RawData),
        {ok, Term} = erl_parse:parse_term(Tokens),
        Term
    catch
        _:_ ->
            {error, could_not_parse_command}
    end.
