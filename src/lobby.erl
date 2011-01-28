-module(lobby).
-behavior(gen_server).

-include("../include/reversi.hrl").

%% API
-export([
         start_link/0,
         client_command/1
        ]).

%% gen_server callbacks
-export([
         init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

-record(lobby_state,
        {
          players = [],
          ready = [],
          games = []
        }).

-record(player,
        {
          name,
          pid
        }).


%%% API

start_link() ->
    gen_server:start_link({local, reversi_lobby}, ?MODULE, [], []).

%% This functions is called by the client handler.
client_command(Command) ->
    gen_server:call(reversi_lobby, {cmd, Command}).

%% enter(Name) ->
%%     gen_server:call(reversi_lobby, {enter, Name}).


%%% gen_server callbacks

init(_Args) ->
    {ok, #lobby_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({cmd, Command}, From, State) ->
    handle_client_command(Command, From, State);
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


%%% Internal functions

handle_client_command({{game, GameID, Command}, _IP}, From, #lobby_state{games = Gs} = LS) ->
    case lists:keyfind(GameID, 1, Gs) of
        {GameID, Server} -> handle_client_game_command(Server, From, Command, LS);
        false            -> {reply, {error, unknown_game}, LS}
    end;

handle_client_command({{login, User, Passwd}, IP}, From, #lobby_state{players = Ps} = LS) ->
    check_inputs,
    case rev_bot:login(User, Passwd, IP) of
        {ok, _} ->
            {reply, {ok, welcome}, LS#lobby_state{players = [From | Ps]}};
        Error   ->
            {reply, Error, LS}
    end;

handle_client_command({{logout}, _IP}, {From,_}, #lobby_state{players = Ps, ready = RPs} = LS) ->
    {reply, good_bye, LS#lobby_state{players = lists:delete(From, Ps),
                                     ready = lists:delete(From, RPs)}};

handle_client_command({{register, User, Player, Desc, Email}, IP},
                      From, #lobby_state{players = Ps} = LS) ->
    check_inputs,
    case rev_bot:register(User, Player, Desc, Email, IP, []) of
        {ok, PW} ->
            {reply, {ok, {password, PW}}, LS#lobby_state{players = [From | Ps]}};
        Error    ->
            {reply, Error, LS}
    end;

handle_client_command({{i_want_to_play}, _IP}, {From, _}, #lobby_state{ready = RPs, games = Gs} = LS) ->
    NewLS =
        case RPs of
            [OtherPlayer] ->
                {ok, #game{id = GameID}} = rev_game_db:new_game(),
                {ok, GameServer} =
                    game_server_sup:start_game_server(GameID, self()),
                NewLS = LS#lobby_state{ready = [],
                                       games = [{GameID, GameServer} | Gs]},
                gen_server:cast(OtherPlayer,
                                {redirect, {lets_play, GameServer, GameID}}),
                {reply, {redirect, {lets_play, GameServer, GameID}}, NewLS};
            [] ->
                NewLS = LS#lobby_state{ready = [From]},
                {reply, {ok, waiting_for_challenge}, NewLS}
        end;


handle_client_command({{list_games}, _IP}, _From, #lobby_state{games = Games} = LS) ->
    {reply, {ok, [Id || {Id, GameServer} <- Games]}, LS};

handle_client_command(_Command, From, LS) ->
    {reply, {error, unknown_command}, LS}.

handle_client_game_command(_GameServer, From, _Command, LS) ->
    {reply, {error, unknown_game_command}, LS}.
