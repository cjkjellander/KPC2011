-module(lobby).
-behavior(gen_server).

-include("../include/reversi.hrl").

%% API
-export([
         start_link/0,
         client_command/1,
         game_over/1,
         game_crash/2
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

game_over(Winner) ->
    gen_server:cast(reversi_lobby, {game_over, self(), Winner}).

game_crash(Reason, GameState) ->
    gen_server:cast(reversi_lobby, {game_server_crash, Reason, GameState}).

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

handle_client_command({game, GameID, Command}, From, #lobby_state{games = Gs} = LS) ->
    case lists:keyfind(GameID, #duel.game_id, Gs) of
        G = #duel{} ->
            handle_client_game_command(G, From, Command, LS);
        false ->
            {reply, {error, unknown_game}, LS}
    end;

handle_client_command({login, User, Passwd}, From, #lobby_state{players = Ps} = LS) ->
    do_login_stuff,
    {reply, welcome, LS#lobby_state{players = [From | Ps]}};

handle_client_command({logout}, From, #lobby_state{players = Ps, ready = RPs} = LS) ->
    {reply, good_bye, LS#lobby_state{players = lists:delete(From, Ps),
                                     ready = lists:delete(From, RPs)}};

handle_client_command({register, User, Passwd}, From, #lobby_state{players = Ps} = LS) ->
    do_register_stuff,
    {reply, welcome, LS#lobby_state{players = [From | Ps]}};

handle_client_command({i_want_to_play}, From, #lobby_state{ready = RPs, games = Gs} = LS) ->
    NewLS =
        case RPs of
            [OtherPlayer | Ps] ->
                %% Opponent found, set up a new game!
                Black = OtherPlayer,
                White = From,
                {ok, Game} = rev_game_db:new_game(),
                GameID = #game.id,
                {ok, GameServer} = game_server_sup:start_game_server(GameID, Black, White),
                G = #duel{game_id = GameID,
                          game_server = GameServer,
                          game_data = Game,
                          black = Black,
                          white = White},
                %% FIXME: Inform players!
                LS#lobby_state{ready = Ps, games = [G | Gs]};
            [] ->
                LS#lobby_state{ready = [From]}
        end,
    {reply, get_ready_for_some_action, NewLS};

handle_client_command({list_games}, _From, #lobby_state{games = Games} = LS) ->
    {reply, {ok, [Id || #duel{game_id = Id} <- Games]}, LS};

handle_client_command(_Command, From, LS) ->
    {reply, {error, unknown_command}, LS}.


handle_client_game_command(#duel{}, _From, _Command, LS) ->
    {reply, {error, unknown_game_command}, LS}.
