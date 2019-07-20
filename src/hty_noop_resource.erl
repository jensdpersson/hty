-module(hty_noop_resource).

-export([handle/2]).

handle(Htx, _Cfg) ->
  Htx.
