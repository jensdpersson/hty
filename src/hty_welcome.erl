-module(hty_welcome).
-export([invoke_list/3]).

invoke_list(Htx, Fspath1, Welcome) when is_tuple(Welcome) ->
  Module = element(1, Welcome),
  Module:list(Htx, Fspath1, Welcome);
invoke_list(Htx, Fspath1, Welcome) ->
  Welcome:list(Htx, Fspath1).
