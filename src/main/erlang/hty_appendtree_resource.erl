-module(hty_appendtree_resource).

-export([new/1, handle/2, mount/1]).

-record(hty_appendtree_resource, {fspath}).

mount(Fspath) ->
  {ok, new(Fspath)}.

new(Fspath) ->
  #hty_appendtree_resource{fspath=Fspath}.

handle(Htx, _This) ->
  case Htx:method() of
    'GET' -> serve(Htx);
    'POST' -> save(Htx);
    _ -> Htx:method_not_allowed(['GET', 'POST'])
  end.


serve(_Htx) ->
  notyet.

save(_Htx) ->
  notyet.
