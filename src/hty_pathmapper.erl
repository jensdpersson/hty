-module(hty_pathmapper).
-export([htx_to_fspath/2, htx_to_fspath/3]).

htx_to_fspath(Htx, Key) ->
  htx_to_fspath(Htx, Key, hty_tx:path_below(Htx)).
  
htx_to_fspath(Htx, Key, Path) ->
  case hty_tx:bound(Key, Htx) of
    {ok, [Pathmapper]} ->
      {ok, hty_storage:invoke_tofs(Path, Pathmapper)};
    _ ->
      {no, "Pathmapper not bound as " ++ Key}
  end.