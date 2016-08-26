-module(hty_chgmethod_resource).
-export([mount/1, handle/2]).
-record(hty_chgmethod_resource, {method, subs}).

mount(Fspath) ->
  Method = case Fspath:parts() of
    [Meth, "chgmethod"] ->
      list_to_atom(Meth);
    ["chgmethod"] ->
      'GET'
  end,
  case hty_mounter:walk(Fspath, "resource") of
    {ok, Subs} ->
      {ok, #hty_chgmethod_resource{method=Method, subs=Subs}};
    {error, Error} ->
      {error, {hty_chgmethod_resource, Error}}
  end.

handle(Htx, This) ->
  Method = This#hty_chgmethod_resource.method,
  Htx1 = Htx:method(),
  Htx1:dispatch(This#hty_chgmethod_resource.subs).
