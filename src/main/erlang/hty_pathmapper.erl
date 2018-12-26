-module(hty_pathmapper).
-export([htx_to_fspath/2]).

htx_to_fspath(Htx, Key) ->
  case Htx:bound(Key) of
    {ok, [Pathmapper]} ->
      {ok, Pathmapper:tofs(Htx:path_below())};
    _ ->
      {no, "Pathmapper not bound as " ++ Key}
  end.
