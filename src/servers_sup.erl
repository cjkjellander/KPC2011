-module(servers_sup).
-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).

%% API
-export([
         start_link/0
        ]).


%%% Supervisor callbacks

init(_Args) ->
    {ok,
     {
       {one_for_one, 5, 2000},
       [
        lobby_child_spec(),
        rev_game_db_child_spec()
       ]
     }}.


%%% API

start_link() ->
    supervisor:start_link({local, reversi_servers_supervisor}, ?MODULE, []).


%%% Internal functions

lobby_child_spec() ->
    {
      lobby,
      {lobby, start_link, []},
      permanent,
      5000,
      worker,
      [lobby]
    }.

rev_game_db_child_spec() ->
    {
      rev_game_db,
      {rev_game_db, start_link, []},
      permanent,
      5000,
      worker,
      [rev_game_db]
    }.
