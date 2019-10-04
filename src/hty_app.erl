-module(hty_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  hty_sup:start_link().

stop(_State) ->
  ok.
