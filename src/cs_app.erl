-module(cs_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_PORT, 8080).

start(_StartType, _StartArgs) ->
  Port = case application:get_env(contest_server, port) of
           {ok, P} -> P;
           undefined -> ?DEFAULT_PORT
         end,
  {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
  case cs_sup:start_link(LSock) of
    {ok, Pid} ->
      cs_sup:start_child(),
      {ok, Pid};
    Other ->
      {error, Other}
  end.

stop(_State) ->
  ok.

