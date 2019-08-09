-module(hty_pathmapper).
-export([htx_to_fspath/2]).

htx_to_fspath(Htx, Key) ->
  case Htx:bound(Key) of
    {ok, [Pathmapper]} ->
      {ok, hty_storage:invoke_tofs(Htx:path_below(), Pathmapper)};
    _ ->
      {no, "Pathmapper not bound as " ++ Key}
  end.
