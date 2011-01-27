-module(cs_server).
-behaviour(gen_server).

-export([start_link/1]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {lsock, user}).

start_link(LSock) ->
  gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
  {ok, #state{lsock = LSock}, 0}.

handle_call(Msg, _From, State) ->
  {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
  NewState = handle_data(Socket, RawData, State),
  {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
  {ok, _Sock} = gen_tcp:accept(LSock),
  cs_sup:start_child(),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
handle_data(Socket, RawData, State) ->
  case State of
    #state{user = undefined} ->
      {Response, NewState} = handler(RawData, State),
      send_msg(Socket, Response),
      NewState;
    #state{user = User} ->
      % TODO forward call
      State
  end.

handler(RawData, State) ->
  Msg = parse_data(RawData),
  {Response, NewState} = case Msg of
                           {login,
                            [{username, Username},{password, Passwd}]} ->
                             case login(Username, Passwd) of
                               true  ->
                                 {ok, State#state{user = Username}};
                               false ->
                                 {{error, login_failed}, State}
                             end;
                           {register,
                            [{username, Username},{password, Passwd}]} ->
                             case register_user(Username, Passwd) of
                               true  ->
                                 {ok, State#state{user = Username}};
                               false ->
                                 {{error, registration_failed}, State}
                             end;
                           logout ->
                             {ok, State#state{user = undefined}};
                           end_of_line ->
                             {end_of_line, State};
                           _  ->
                             {{error, unknown_command}, State}
                         end,
  {lists:flatten(io_lib:format("~p", [Response])), NewState}.

parse_data("\n") ->
  end_of_line;
parse_data(RawData) ->
  %% FIXME add error handling
  {ok, Tokens, _} = erl_scan:string(RawData),
  {ok, Term} = erl_parse:parse_term(Tokens),
  Term.

login(Username, Password) ->
  case cs_db:get_password(Username) of
    {ok, StoredPassword} ->
      StoredPassword =:= Password;
    _ -> false
  end.

register_user(Username, Password) ->
  cs_db:add_user(Username, Password).

send_msg(Socket, Msg) ->
  gen_tcp:send(Socket, Msg ++ "\n").
