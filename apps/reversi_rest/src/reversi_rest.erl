%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc reversi_rest startup code

-module(reversi_rest).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    reversi_rest_sup:start_link().

%% @spec start() -> ok
%% @doc Start the reversi_rest server.
start() ->
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(reversi_rest).

%% @spec stop() -> ok
%% @doc Stop the reversi_rest server.
stop() ->
    Res = application:stop(reversi_rest),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    Res.
