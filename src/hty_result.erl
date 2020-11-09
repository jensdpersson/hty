-module(hty_result).
-export([assume_ok/1]).

assume_ok(Result) ->
  {ok, Ok} = Result,
  Ok.
