-module(hty_realm).
-export([invoke_auth/3, invoke_name/1]).

invoke_auth(Nick, Pass, Realm) ->
  Module = element(1, Realm),
  Module:auth(Nick, Pass, Realm).

invoke_name(Realm) ->
  Module = element(1, Realm),
  Module:name(Realm).
