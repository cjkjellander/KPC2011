-module(client_handler_tcp).
-behaviour(gen_server).

-export([start_link/1]).

-export([ init/1
          , handle_call/3
          , handle_cast/2
          , handle_info/2
          , terminate/2
          , code_change/3
        ]).

-define(DEFAULT_PORT, 7676).

-record(state, {lsock
                , ip
                , game_server
                , socket
               }).

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Response, #state{socket=Socket} = State) ->
    NewState = handle_response(Response, Socket, State),
    {noreply, NewState}.

handle_info({tcp, Socket, RawData}, State) ->
    NewState = handle_data(Socket, RawData, State),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    {ok, {IP, _Port}} = inet:sockname(Socket),
    %% FIXME: Do stuff with the IP-address.
    client_handler_sup:start_client_handler(?MODULE, [LSock]),
    {noreply, State#state{ip = IP, socket=Socket}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
handle_data(Socket, RawData, #state{ip = IP, game_server=undefined} = State) ->
    Request = parse_data(RawData),
    case Request of
        [] ->
            %% do nothing
            State;
        {error, could_not_parse_command} ->
            send_msg(Socket, term_to_string(Request)),
            State;
        _ ->
            Response = lobby:client_command({Request, IP}),
            handle_response(Response, Socket, State)
    end;

handle_data(Socket, RawData, #state{ip = _IP, game_server=GS} = State) ->
    Request = parse_data(RawData),
    case Request of
        [] ->
            %% do nothing
            State;
        {error, could_not_parse_command} ->
            send_msg(Socket, term_to_string(Request)),
            State;
        _ ->
            Response = game_server:client_command(GS, Request),
            handle_response(Response, Socket, State)
    end.

handle_response({redirect, {lets_play, GS, Gid}}, Socket, State) ->
    send_msg(Socket, term_to_string({ok, {lets_play, GS, Gid}})),
    State#state{game_server=GS};

handle_response(Response, Socket, State) ->
    send_msg(Socket, term_to_string(Response)),

    if Response =:= good_bye ->
            gen_tcp:close(Socket);
       true ->
            ok
    end,

    State.



parse_data("\n") ->
    [];
parse_data(RawData) ->
    try
        {ok, Tokens, _} = erl_scan:string(RawData),
        {ok, Term} = erl_parse:parse_term(Tokens),
        Term
    catch
        _:_ ->
            {error, could_not_parse_command}
    end.

term_to_string(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

send_msg(Socket, Msg) ->
    gen_tcp:send(Socket, Msg ++ "\n").
