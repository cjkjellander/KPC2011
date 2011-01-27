-module(lobby).
-behavior(gen_server).
-export([start_link/0
         , stop/0
         ]).

start_link() ->
    ok.

stop() ->
    ok.
