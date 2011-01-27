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
  {Response, NewState} = handler(RawData, State),
  gen_tcp:send(Socket, Response),
  NewState.

handler(RawData, State) ->
  Msg = parse_data(RawData),
  {Response, NewState} = case Msg of
                           {login,
                            [{username, Username},{password, Passwd}]} ->
                             % validate user, update state
                             case login(Username, Passwd) of
                               true  ->
                                 {"ok.\n", State#state{user = Username}};
                               false ->
                                 {"false.\n", State}
                             end;
                           {register,
                            [{username, Username},{password, Passwd}]} ->
                             case register_user(Username, Passwd) of
                               true  ->
                                 {"ok.\n", State#state{user = Username}};
                               false ->
                                 {"false.\n", State}
                             end;
                           _  ->
                             {"false.\n", State}
                         end,
  {Response, NewState}.

parse_data(RawData) ->
  {ok, Tokens, _} = erl_scan:string(RawData),
  {ok, Term} = erl_parse:parse_term(Tokens),
  Term.

login(_Username, _Passwd) ->
  true.

register_user(_Username, _Passwd) ->
  true.
