-module(rand_bot_sup).
-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).

%% API
-export([
         start_link/4
        ]).


%%% Supervisor callbacks

init({Host, Port, Name, Passwd}) ->
    {ok,
     {
       {rest_for_one, 2, 2000},
       [
        servers_sup_child_spec(Host, Port, Name, Passwd)
       ]
     }}.


%%% API

start_link(Host, Port, Name, Passwd) ->
    supervisor:start_link({local, reversi_supervisor}, ?MODULE,
                         {Host, Port, Name, Passwd}).


%%% Internal functions


servers_sup_child_spec(Host, Port, Name, Passwd) ->
    {
      rand_bot,
      {rand_bot, start, [Host, Port, Name, Passwd]},
      permanent,
      5000,
      supervisor,
      [rand_bot]
    }.
