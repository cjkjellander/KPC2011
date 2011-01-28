-module(reversi_app).
-behaviour(application).

-export([start/2, stop/1]).


start(normal, _Args) ->
    %% FIXME: Make sure database tables exists and so on
    reversi_sup:start_link().

stop(_State) ->
    ok.
