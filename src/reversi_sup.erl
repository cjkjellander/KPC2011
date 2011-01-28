-module(reversi_sup).
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
       {rest_for_one, 2, 2000},
       [
        servers_sup_child_spec(),
        client_handler_sup_child_spec(),
        game_server_sup_child_spec()
       ]
     }}.


%%% API

start_link() ->
    supervisor:start_link({local, reversi_supervisor}, ?MODULE, []).


%%% Internal functions


servers_sup_child_spec() ->
    {
      servers_sup,
      {servers_sup, start_link, []},
      permanent,
      5000,
      supervisor,
      [servers_sup]
    }.

client_handler_sup_child_spec() ->
    {
      client_handler_sup,
      {client_handler_sup, start_link, []},
      permanent,
      5000,
      supervisor,
      [client_handler_sup]
    }.

game_server_sup_child_spec() ->
    {
      game_server_sup,
      {game_server_sup, start_link, []},
      permanent,
      5000,
      supervisor,
      [game_server_sup]
    }.
