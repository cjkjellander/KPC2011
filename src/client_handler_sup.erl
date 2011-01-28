-module(client_handler_sup).
-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).

%% API
-export([
         start_link/0,
         start_client_handler/1,
         start_client_handler/2
        ]).


%%% Supervisor callbacks

init(_Args) ->
    {ok,
     {
       {one_for_one, 1, 5000},
       []
     }}.


%%% API

start_link() ->
    supervisor:start_link({local, reversi_client_handler_supervisor},
                          ?MODULE,
                          []).

start_client_handler(Module) ->
    start_client_handler(Module, []).

start_client_handler(Module, Args) ->
    supervisor:start_child(reversi_client_handler_supervisor,
                           client_handler_child_spec(Module, Args)).


%%% Internal functions

client_handler_child_spec(Module, Args) ->
    {
      client_handler,
      {Module, start_link, Args},
      temporary,
      5000,
      worker,
      [Module]
    }.
