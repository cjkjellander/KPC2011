-module(game_server_sup).
-behavior(supervisor).

%% Supervisor callbacks
-export([init/1]).

%% API
-export([
         start_link/0,
         start_game_server/1
        ]).


%%% Supervisor callbacks

init(_Args) ->
    {ok,
     {
       {simple_one_for_one, 5, 2000},
       [{game_server,
         {game_server, start_link, []},
         transient,
         1000,
         worker,
         [game_server]
        }]
     }}.


%%% API

start_link() ->
    supervisor:start_link({local, reversi_game_server_supervisor}, ?MODULE, []).

start_game_server(GameNumber) ->
    supervisor:start_child(reversi_game_server_supervisor,
                           [GameNumber]).
