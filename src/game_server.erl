-module(game_server).
-behavior(gen_fsm).
-version('0.1').

-export([start_link/1
        ]).

%% External Interface
-export([login/2
         , move/4
         , status/1
         , opponent/1
        ]).

% Internal Interface
-export([init/1
         , code_change/4
         , handle_event/3
         , handle_info/3
         , handle_sync_event/4
         , terminate/3
        ]).
-export([setup/3
         , black_ready/3
         , white_ready/3
         , play/3
         , this_guy/2
         , p2a/1
        ]).

-include("../include/reversi.hrl").

-record(game_state, {game, black, white, lobby}).

start_link({N, Lobby}) ->
    gen_fsm:start_link(?MODULE, {N, Lobby}, []).

init({N, Lobby}) ->
    G = reversi:new_game(N),
    GS = #game_state{game=G, lobby=Lobby},
    {ok, setup, GS}.

setup({login, ?B}, Pid, GS) ->
    {reply, {ok, logged_in}, black_ready, GS#game_state{black=Pid}};
setup({login, ?W}, Pid, GS) ->
    {reply, {ok, logged_in}, white_ready, GS#game_state{white=Pid}};
setup(_, _Pid, GS) ->
    {reply, {error, imsorrydavecantdothat}, setup, GS}.

black_ready({login, ?W}, Pid, GS) ->
    {reply, {ok, logged_in}, play, GS#game_state{white=Pid}};
black_ready(_, _, GS) ->
    {reply, {error, imsorrydavecantdothat}, black_ready, GS}.

white_ready({login, ?B}, Pid, GS) ->
    {reply, {ok, logged_in}, play, GS#game_state{black=Pid}};
white_ready(_, _, GS) ->
    {reply, {error, imsorrydavecantdothat}, white_ready, GS}.

play({move, Who, X, Y}, Pid, #game_state{game=#game{togo=Who}} = GS) ->
    case reversi:move(GS#game_state.game, X, Y, Who) of
        {ok, NewG} ->
            which_state(Who, NewG, Pid, GS);
        Error ->
            {reply, Error, play, GS}
    end.

which_state(Who, Game, _Pid, #game_state{lobby=L} = GS) ->
    case reversi:move_check(Game) of
        {go, Game, _} ->
            gen_server:cast(that_guy(GS, Who),
                            {your_move, Game}),
            {reply, {ok, please_wait}, play, GS#game_state{game=Game}};
        {switch, NewGame, _} ->
            gen_server:cast(that_guy(GS, Who),
                            {nothing_to_do, Game}),
            {reply, {your_move, NewGame#game.board}, play, GS#game_state{game=Game}};
        {done, G, Winner} ->
            gen_server:cast(L, {game_over, G, Winner}),
            gen_server:cast(that_guy(GS, Who),
                            {game_over, G, Winner}),
            {stop, {game_over, G, Winner}, GS#game_state{game=G}}
    end.

this_guy(#game_state{black=B}, ?B) -> B;
this_guy(#game_state{white=W}, ?W) -> W.

that_guy(#game_state{black=B}, ?W) -> B;
that_guy(#game_state{white=W}, ?B) -> W.


p2a(?B) -> black;
p2a(?W) -> white;
p2a(?E) -> adraw.

%% Your behavior is despicable!

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(game_status, _From, StateName, GS) ->
    {reply, GS#game_state.game, StateName, GS};
handle_sync_event(opponent, From, StateName, #game_state{black = From, white = Opponent} = GS) ->
    {reply, Opponent, StateName, GS};
handle_sync_event(opponent, From, StateName, #game_state{black = Opponent, white = From} = GS) ->
    {reply, Opponent, StateName, GS};
handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(Reason, _, #game_state{lobby=L} = GS) ->
    gen_server:cast(L, {crash, Reason, GS}).


%% Interface functions

login(GameServer, Color) ->
    gen_fsm:sync_send_event(GameServer, {login, Color}).

move(GameServer, Color, X, Y) ->
    gen_fsm:sync_send_event(GameServer, {move, Color, X, Y}).

status(GameServer) ->
    gen_fsm:sync_send_all_event(GameServer, game_status).

opponent(GameServer) ->
    gen_fsm:sync_send_all_event(GameServer, opponent).
