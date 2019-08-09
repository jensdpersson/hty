-module(hty_bind_resource).
-record(hty_bind_resource, {key, bound}).

-export([mount/1, new/2, handle/2]).

mount(Fspath) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["bind", Params|_] ->
      {Key, Type} = parseParams(Params),
      case hty_mounter:walk(Fspath, Type) of
        {ok, Subs} ->
          {ok, new(Key, Subs)};
        {error, _} = Error ->
          Error
      end;
    ["bind"] ->
      {error, "bind resource needs a parameter to use as the binding key"}
  end.

new(Key, Bound) ->
  #hty_bind_resource{key=Key, bound=Bound}.

handle(Htx, This) ->
  hty_tx:bind(This#hty_bind_resource.key, This#hty_bind_resource.bound, Htx).

parseParams(Params) ->
  case string:tokens(Params, ",") of
    [Key] ->
      {Key, "resource"};
    [Key, Type|_] ->
      {Key, Type}
  end.
