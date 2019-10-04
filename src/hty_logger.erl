-module(hty_logger).
-export([invoke_grep/6]).

invoke_grep(From, To, Pattern, Before, After, Logger) ->
  Module = element(1, Logger),
  Module:grep(From, To, Pattern, Before, After, Logger).
