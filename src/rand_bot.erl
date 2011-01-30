-module(rand_bot).

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
    io:format("~s~n", [Login]),
    gen_tcp:send(Sock, Login),
    {value, {_,_,Reply}} = wait_reply(Sock),
    io:format("~s~n", [binary_to_list(Reply)]).



wait_reply(_Timeout) ->
    receive
	Reply ->
	    {value, Reply}
    after 1000 ->
	    timeout
    end.

mk_login(Name, Passwd) ->
    io_lib:format("{login,\"~s\",\"~s\"}.", [Name, Passwd]).

