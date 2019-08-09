-module(hty_storage).
-export([invoke_tofs/2]).

invoke_tofs(Path, Storage) ->
  Module = element(1, Storage),
  Module:tofs(Path, Storage).
