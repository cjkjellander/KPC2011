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

handle_cast({game_over, #game{id = ID} = G, _Winner}, State) ->
    %% TODO: Update player rankings.
    rev_game_db:update_game(G),
    lobby_db:delete_game(ID),
    {noreply, State};
handle_cast({game_crash, GameServer, _Black, _White}, State) ->
    %% TODO: Remove game from database (or store crash info?)
    lobby_db:delete_game_server(GameServer),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


%%% Internal functions

handle_client_command({{game, GameID, Command}, _IP}, From, State) ->
    case lobby_db:read_game(GameID) of
        [G = #duel{}] ->
            handle_client_game_command(G, From, Command, State);
        [] ->
            {reply, {error, unknown_game}, State}
    end;

handle_client_command({{login, User, Passwd}, IP}, From, State) ->
    check_inputs,
    case rev_bot:login(User, Passwd, IP) of
        {ok, _} ->
            ok = lobby_db:add_player(From),
            {reply, {ok, welcome}, State};
        Error   ->
            {reply, Error, State}
    end;

handle_client_command({{logout}, _IP}, {From,_}, State) ->
    lobby_db:delete_player(From),
    {reply, good_bye, State};

handle_client_command({{register, User, Player, Desc, Email}, IP},
                      From, State) ->
    check_inputs,
    case rev_bot:register(User, Player, Desc, Email, IP, []) of
        {ok, PW} ->
            lobby_db:add_player(From),
            {reply, {ok, {password, PW}}, State};
        Error    ->
            {reply, Error, State}
    end;

handle_client_command({{i_want_to_play}, _IP}, {From, _}, State) ->
    case lobby_db:find_ready_player() of
        [OtherPlayer] ->
            %% Opponent found, set up a new game!
            {ok, Game} = rev_game_db:new_game(),
            GameID = Game#game.id,
            {ok, GameServer} = game_server_sup:start_game_server(GameID),
            G = #duel{game_id = GameID,
                      game_server = GameServer,
                      game_data = Game},
            lobby_db:add_game(G),
            gen_server:cast(OtherPlayer,
                            {redirect, {lets_play, GameServer, ?B, GameID}}),
            {reply, {redirect, {lets_play, GameServer, ?W, GameID}}, State};
        [] ->
            lobby_db:add_ready_player(From),
            {reply, {ok, waiting_for_challenge}, State}
    end;

handle_client_command({{list_games}, _IP}, _From, State) ->
    Games = lobby_db:list_games(),
    {reply, {ok, [Id || #duel{game_id = Id} <- Games]}, State};

handle_client_command(_Command, _From, LS) ->
    {reply, {error, unknown_command}, LS}.


handle_client_game_command(#duel{}, _From, _Command, LS) ->
    {reply, {error, unknown_game_command}, LS}.

cookie() ->
    <<A:64>> = crypto:rand_bytes(8),
    A.
