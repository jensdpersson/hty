-module(hty_chgmethod_resource).
-export([mount/2, handle/2]).
-record(hty_chgmethod_resource, {method, subs}).

mount(Fspath, Mc) ->
  Method = case hty_fspath:parts(Fspath) of
    [Meth, "chgmethod"] ->
      list_to_atom(string:uppercase(Meth));
    ["chgmethod"] ->
      'GET'
  end,
  case hty_mounter:walk(Fspath, "resource", Mc) of
    {ok, Subs} ->
      {ok, #hty_chgmethod_resource{method=Method, subs=Subs}};
    {error, Error} ->
      {error, {hty_chgmethod_resource, Error}}
  end.

handle(Htx, This) ->
  Method = This#hty_chgmethod_resource.method,
  hty_tx:dispatch(This#hty_chgmethod_resource.subs, hty_tx:method(Method, Htx)).
