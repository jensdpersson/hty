-module(hty_resource).
-export([invoke_handle/2, invoke_mount/2]).

invoke_handle(Htx, Record) ->
  Module = element(1, Record),
  Module:handle(Htx, Record).

invoke_mount(Fspath, Record) ->
  %Module = element(1, Record),
  Record:mount(Fspath).
