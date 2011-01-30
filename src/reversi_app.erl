-module(reversi_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_PORT, 7676).

start(normal, _Args) ->
    Port = case application:get_env(reversi, port) of
               {ok, P} -> P;
               undefined -> ?DEFAULT_PORT
           end,
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]),
    %% FIXME: Make sure database tables exists and so on
    reversi_sup:start_link(LSock).

stop(_State) ->
    ok.
