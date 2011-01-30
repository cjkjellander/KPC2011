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

-define(DEFAULT_PORT, 7676).

%%% Supervisor callbacks

init(LSock) ->
    {ok,
     {
       {one_for_one, 1, 5000},
       [client_handler_child_spec(client_handler_tcp, [LSock])]
     }}.


%%% API

start_link() ->
    Port = case application:get_env(reversi, port) of
               {ok, P} -> P;
               undefined -> ?DEFAULT_PORT
           end,
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]),
    supervisor:start_link({local, reversi_client_handler_supervisor},
                          ?MODULE,
                          LSock).

start_client_handler(Module) ->
    start_client_handler(Module, []).

start_client_handler(Module, Args) ->
    supervisor:start_child(reversi_client_handler_supervisor,
                           client_handler_child_spec(Module, Args)).


%%% Internal functions

client_handler_child_spec(Module, Args) ->
    {
      {client_handler, make_ref()},
      {Module, start_link, Args},
      temporary,
      5000,
      worker,
      [Module]
    }.
