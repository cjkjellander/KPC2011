-module(lobby).
-behavior(gen_server).

-include("../include/reversi.hrl").

%% API
-export([
         start_link/0
         , client_command/1
         , game_over/2
         , game_crash/4
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

%% An ongoing game.
-record(duel,
        {
          game_id,
          game_server,
          game_data,
          black,
          white
        }).


%%% API

start_link() ->
    gen_server:start_link({local, reversi_lobby}, ?MODULE, [], []).

%% This functions is called by the client handler.
client_command(Command) ->
    gen_server:call(reversi_lobby, {cmd, Command}).

game_over(Game, Winner) ->
    gen_server:cast(reversi_lobby, {game_over, Game, Winner}).

game_crash(Reason, Game, Black, White) ->
    gen_server:cast(reversi_lobby,
                    {game_server_crash, Reason, Game, Black, White}).


%%% gen_server callbacks

init(_Args) ->
    {ok, #lobby_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({cmd, Command}, From, State) ->
    handle_client_command(Command, From, State);
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({game_over, #game{id = ID} = G, _Winner}, #lobby_state{games = Gs} = LS) ->
    %% TODO: Update player rankings.
    rev_game_db:update_game(G),
    NewGs = lists:keydelete(ID, #duel.game_id, Gs),
    {noreply, LS#lobby_state{games = NewGs}};
handle_cast({game_crash, GameServer, _Black, _White}, #lobby_state{games = Gs} = LS) ->
    %% TODO: Remove game from database (or store crash info?)
    NewGs = lists:keydelete(GameServer, #duel.game_server, Gs),
    {noreply, LS#lobby_state{games = NewGs}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


%%% Internal functions

handle_client_command({{game, GameID, Command}, _IP}, From, #lobby_state{games = Gs} = LS) ->
    case lists:keyfind(GameID, #duel.game_id, Gs) of
        G = #duel{} ->
            handle_client_game_command(G, From, Command, LS);
        false ->
            {reply, {error, unknown_game}, LS}
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
    case RPs of
        [OtherPlayer | Ps] ->
            %% Opponent found, set up a new game!
            {ok, Game} = rev_game_db:new_game(),
            GameID = Game#game.id,
            B = cookie(),
            W = cookie(),
            {ok, GameServer} = game_server_sup:start_game_server(GameID,B,W),
            G = #duel{game_id = GameID,
                      game_server = GameServer,
                      game_data = Game,
                      black = B,
                      white = W
                     },
            NewLS = LS#lobby_state{ready = Ps, games = [G | Gs]},
            gen_server:cast(OtherPlayer,
                            {redirect, {lets_play, GameServer, ?B, GameID, B}}),
            {reply, {redirect, {lets_play, GameServer, ?W, GameID, W}}, NewLS};
        [] ->
            NewLS = LS#lobby_state{ready = [From]},
            {reply, {ok, waiting_for_challenge}, NewLS}
    end;

handle_client_command({{list_games}, _IP}, _From, #lobby_state{games = Games} = LS) ->
    {reply, {ok, [Id || #duel{game_id = Id} <- Games]}, LS};

handle_client_command(_Command, _From, LS) ->
    {reply, {error, unknown_command}, LS}.


handle_client_game_command(#duel{}, _From, _Command, LS) ->
    {reply, {error, unknown_game_command}, LS}.

cookie() ->
    <<A:64>> = crypto:rand_bytes(8),
    A.
