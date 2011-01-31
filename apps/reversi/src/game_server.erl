-module(game_server).
-behavior(gen_fsm).
-version('0.1').

-export([start_link/3
        ]).

%% External Interface
-export([login/2
         , move/4
         , status/1
         , opponent/1
         , client_command/2
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

-record(game_state, {game, black, white}).

-record(who, {pid, cookie}).

start_link(N,BCookie,WCookie) ->
    gen_fsm:start_link(?MODULE, {N,BCookie,WCookie}, []).

init({N,BCookie,WCookie}) ->
    {ok, G} = rev_game_db:get_game(N),
    GS = #game_state{  game=G
                     , black=#who{cookie=BCookie}
                     , white=#who{cookie=WCookie}},
    {ok, setup, GS}.


setup({login, Cookie, ?B}, {Pid,_},
      #game_state{black=#who{cookie=Cookie}} = GS) ->
    {reply, {ok, wait_for_other_guy}, black_ready,
     GS#game_state{black=#who{pid=Pid,cookie=Cookie}}};
setup({login, Cookie, ?W}, {Pid,_},
      #game_state{white=#who{cookie=Cookie}} = GS) ->
    {reply, {ok, wait_for_other_guy}, white_ready,
     GS#game_state{white=#who{pid=Pid,cookie=Cookie}}};
setup({login, _Cookie, _}, _, GS) ->
    {reply, {error, badcookie}, setup, GS};
setup(_, _Pid, GS) ->
    {reply, {error, imsorrydavecantdothat}, setup, GS}.

black_ready({login, Cookie, ?W = Who}, {Pid,_},
            #game_state{game=Game,white=#who{cookie=Cookie}} = GS) ->
    timer:sleep(500),
    gen_server:cast(that_guy(GS, Who), {ok, {your_move, Game#game{moves=[]}}}),
    timer:sleep(500),
    {reply, {ok, {please_wait, Game#game{moves=[]}}},
     play, GS#game_state{white=#who{pid=Pid,cookie=Cookie}}};
black_ready(_, _, GS) ->
    {reply, {error, imsorrydavecantdothat}, black_ready, GS}.

white_ready({login, Cookie, ?B = Who}, {Pid,_},
            #game_state{game=Game,black=#who{cookie=Cookie}} = GS) ->
    timer:sleep(500),
    gen_server:cast(that_guy(GS, Who), {ok, {please_wait, Game#game{moves=[]}}}),
    timer:sleep(500),
    {reply, {ok, {your_move, Game#game{moves=[]}}},
     play, GS#game_state{black=#who{pid=Pid,cookie=Cookie}}};
white_ready(_, _, GS) ->
    {reply, {error, imsorrydavecantdothat}, white_ready, GS}.

play({move, _Cookie, ?B = Who, X, Y}, {Pid,_},
     #game_state{game=#game{togo=Who},black=#who{cookie=_Cookie}} = GS) ->
    case reversi:move(GS#game_state.game, X, Y, Who) of
        {ok, NewG} ->
            which_state(Who, NewG, Pid, GS);
        Error ->
            {reply, Error, play, GS}
    end;
play({move, _Cookie, ?W = Who, X, Y}, {Pid,_},
     #game_state{game=#game{togo=Who},white=#who{cookie=_Cookie}} = GS) ->
    case reversi:move(GS#game_state.game, X, Y, Who) of
        {ok, NewG} ->
            which_state(Who, NewG, Pid, GS);
        Error ->
            {reply, Error, play, GS}
    end;
play({move, _, Who, _, _}, _, #game_state{game=#game{togo=Who}} = GS) ->
    {reply, {error, badcookie}, play, GS};
play({move, _, _, _, _}, _, GS) ->
    {reply, {error, notyourturn}, play, GS};
play({quit, _Cookie, ?B = Who}, _Pid,
     #game_state{game=Game,black=#who{cookie=_Cookie}} = GS) ->
    GameOver = {redirect, {game_over, Game, ?E}},
    gen_server:cast(that_guy(GS, Who), GameOver),
    {stop, normal, GameOver, GS#game_state{game=Game}};
play({quit, _Cookie, ?W = Who}, _Pid,
     #game_state{game=Game,white=#who{cookie=_Cookie}} = GS) ->
    GameOver = {redirect, {game_over, Game, ?E}},
    gen_server:cast(that_guy(GS, Who), GameOver),
    {stop, normal, GameOver, GS#game_state{game=Game}};
play(_, _Pid, GS) ->
    {reply, {error, imsorrydavecantdothat}, play, GS}.

which_state(Who, Game, _Pid, GS) ->
    case reversi:move_check(Game) of
        {go, Game, _} ->
            timer:sleep(500),
            gen_server:cast(that_guy(GS, Who),
                            {ok, {your_move, Game#game{moves=[]}}}),
            {reply, {ok, {please_wait, Game#game{moves=[]}}},
             play, GS#game_state{game=Game}};
        {switch, NewGame, _} ->
            timer:sleep(500),
            gen_server:cast(that_guy(GS, Who),
                            {ok, {please_wait, NewGame#game{moves=[]}}}),
            timer:sleep(500),
            {reply, {ok, {your_move, NewGame#game{moves=[]}}}, play,
             GS#game_state{game=NewGame}};
        {done, Game, Winner} ->
            lobby:game_over(Game, Winner),
            GameOver = {redirect, {game_over, Game#game{moves=[]}, Winner}},
            timer:sleep(500),
            gen_server:cast(that_guy(GS, Who), GameOver),
            timer:sleep(500),
            {stop, normal, GameOver, GS#game_state{game=Game}}
    end.

this_guy(#game_state{black=#who{pid=B}}, ?B) -> B;
this_guy(#game_state{white=#who{pid=W}}, ?W) -> W;
this_guy(_, ?E)                    -> undefined.

that_guy(#game_state{black=#who{pid=B}}, ?W) -> B;
that_guy(#game_state{white=#who{pid=W}}, ?B) -> W;
that_guy(_, ?E)                    -> undefined.


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

handle_sync_event({availabe_moves, Game, Who}, _From, StateName, GS) ->
    {reply, {ok, reversi:check_avail(Game, Who)}, StateName, GS};
handle_sync_event({game_status}, _From, StateName, GS) ->
    {reply, {ok, GS#game_state.game}, StateName, GS};
handle_sync_event({board}, _From, StateName, #game_state{game = G} = GS) ->
    {reply, {ok, reversi:game2lists(G)}, StateName, GS};
handle_sync_event({opponent}, From, StateName,
                  #game_state{black=#who{pid=From},
                              white=#who{pid=Opponent}} = GS) ->
    {reply, Opponent, StateName, GS};
handle_sync_event({opponent}, From, StateName,
                  #game_state{black=#who{pid=Opponent},
                              white=#who{pid=From}}= GS) ->
    {reply, Opponent, StateName, GS};
handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.


terminate(normal, _, #game_state{}) ->
    ok;
terminate(Reason, _, #game_state{game = Game,
                                 black=#who{pid=B},
                                 white=#who{pid=W}}) ->
    gen_server:cast(B, {redirect, {game_crash, Game}}),
    gen_server:cast(W, {redirect, {game_crash, Game}}),
    lobby:game_crash(Reason, Game, B, W).


%% Interface functions

login(GameServer, Color) ->
    gen_fsm:sync_send_event(GameServer, {login, Color}).

move(GameServer, Who, X, Y) ->
    gen_fsm:sync_send_event(GameServer, {move, Who, X, Y}).

status(GameServer) ->
    gen_fsm:sync_send_all_state_event(GameServer, {game_status}).

opponent(GameServer) ->
    gen_fsm:sync_send_all_state_event(GameServer, {opponent}).

client_command(GameServer, {game_status} = Request) ->
    gen_fsm:sync_send_all_state_event(GameServer, Request);
client_command(GameServer, {opponent} = Request) ->
    gen_fsm:sync_send_all_state_event(GameServer, Request);
client_command(GameServer, {board} = Request) ->
    gen_fsm:sync_send_all_state_event(GameServer, Request);
client_command(GameServer, Request) ->
    gen_fsm:sync_send_event(GameServer, Request).
