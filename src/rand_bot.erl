%%%-------------------------------------------------------------------
%%% File    : rand_bot.erl
%%% Author  : Thomas Järvstrand <>
%%% Description : 
%%%
%%% Created : 30 Jan 2011 by Thomas Järvstrand <>
%%%-------------------------------------------------------------------
-module(rand_bot).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%% States
-export([ connected/2
        , logged_in/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 7676).
-define(DEFAULT_HOST, "localhost").

-record(state, {host = ?DEFAULT_HOST,
                port = ?DEFAULT_PORT,
                lsock,
                name = "foo",
                password = "bar",
                game
               }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([]) ->
    case connect() of
	{ok, Sock} -> {ok, connected, #state{lsock = Sock}};
	Err        -> {stop, Err}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------

connected(_Event, State) ->
    case login(State) of
        welcome -> {logged_in, state_name, State};
        Err     -> {stop, Err, State}
    end.

logged_in(_Event, #state{game = undefined} = State) ->
    case new_game(State) of
        ok -> {logged_in, state_name, State};
        Err  -> {stop, Err, State}
    end.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
%% state_name(_Event, _From, State) ->
%%     Reply = ok,
%%     {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

connect() ->
    connect(?DEFAULT_HOST, ?DEFAULT_PORT).
connect(Host, Port) ->
    gen_tcp:connect(Host, Port, [binary,{packet, 0}]).

login(#state{lsock = LSock, name = Name, password = Password}) ->
    do_rpc(LSock, {login, Name, Password}).

new_game(LSock) ->
    case do_rpc(LSock, {i_want_to_play}) of
        Res -> io:format("~p", Res)
    end.

term_to_string(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

do_rpc(LSock, Msg) ->
    gen_tcp:send(LSock, term_to_string(Msg)),
    case gen_tcp:recv(LSock, 0) of
        {ok, Packet} -> tcp_parse:parse_data(Packet);
        Err -> Err
    end.

%% -module(rand_bot).

%% -behaviour(gen_fsm).

%% -export([
%%          start/4
%%         ]).

%% start(Host, Port, Name, Passwd) ->
%%     case gen_tcp:connect(Host, Port, [binary,{packet, 0}]) of
%% 	{ok, Sock} ->
%%             login(Sock, Name, Passwd),
%% 	    gen_tcp:close(Sock);
%% 	_ ->
%% 	    error
%%     end.

%% login(Sock, Name, Passwd) ->
%%     catch wait_reply(1000),
%%     Login = mk_login(Name, Passwd),
%%     io:format("~s~n", [Login]),
%%     gen_tcp:send(Sock, Login),
%%     {value, {_,_,Reply}} = wait_reply(Sock),
%%     io:format("~s~n", [binary_to_list(Reply)]).



%% wait_reply(_Timeout) ->
%%     receive
%% 	Reply ->
%% 	    {value, Reply}
%%     after 1000 ->
%% 	    timeout
%%     end.

%% mk_login(Name, Passwd) ->
%%     io_lib:format("{login,\"~s\",\"~s\"}.", [Name, Passwd]).

