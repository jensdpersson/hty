-module(hty_pathmapper).
-export([htx_to_fspath/2]).

htx_to_fspath(Htx, Key) ->
  case hty_tx:bound(Key, Htx) of
    {ok, [Pathmapper]} ->
      {ok, hty_storage:invoke_tofs(hty_tx:path_below(Htx), Pathmapper)};
    _ ->
      {no, "Pathmapper not bound as " ++ Key}
  end.
