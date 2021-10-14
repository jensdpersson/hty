-module(hty_pathmapper).
-export([htx_to_fspath/2, htx_to_fspath/3, htx_to_fspath/4]).

htx_to_fspath(Htx, Key) ->
  htx_to_fspath(Htx, Key, hty_tx:path_below(Htx)).

htx_to_fspath(Htx, Key, Path) ->
  htx_to_fspath(Htx, Key, Path, []).
  
htx_to_fspath(Htx, Key, Path, Strip) ->
  case hty_tx:bound(Key, Htx) of
    {ok, [Pathmapper]} ->
        Prefixstripped = strip_prefix(Strip, Path),
      {ok, hty_storage:invoke_tofs(Prefixstripped, Pathmapper)};
    _ ->
      {no, "Pathmapper not bound as " ++ Key}
  end.
  
strip_prefix([X|Xs], [X|Ys]) ->
    io:format("Stripping ~p from ~p", [[X|Xs], [X|Ys]]),
    strip_prefix(Xs, Ys);
strip_prefix(Xs, Ys) -> 
    io:format("Done stripping ~p from ~p", [Xs, Ys]),
    Ys.