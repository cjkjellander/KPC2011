-module(rand_bot_sup).
-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).

%% API
-export([
         start_link/2,
         start_rand_bot/2
        ]).


%%% Supervisor callbacks

init({Host, Port}) ->
    {ok,
     {
       {simple_for_one, 2, 2000},
       [
        rand_bot_child_spec(Host, Port)
       ]
     }}.


%%% API

start_link(Host, Port) ->
    supervisor:start_link({local, reversi_rand_bot_supervisor}, ?MODULE,
                          {Host, Port}).

start_rand_bot(Name, Password) ->
    supervisor:start_child(reversi_rand_bot_supervisor, [Name, Password]).


%%% Internal functions

rand_bot_child_spec(Host, Port) ->
    {
      rand_bot2,
      {rand_bot2, start, [Host, Port]},
      permanent,
      5000,
      worker,
      [rand_bot2]
    }.
