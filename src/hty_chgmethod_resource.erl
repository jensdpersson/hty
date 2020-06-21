-module(hty_chgmethod_resource).
-export([mount/2, handle/2]).
-record(this, {method, subs}).

mount(Fspath, Mc) ->
  Method = case hty_fspath:parts(Fspath) of
    [Meth, "chgmethod"] ->
      list_to_atom(Meth);
    ["chgmethod"] ->
      'GET'
  end,
  case hty_mounter:walk(Fspath, "resource", Mc) of
    {ok, Subs} ->
      {ok, #this{method=Method, subs=Subs}};
    {error, Error} ->
      {error, {hty_chgmethod_resource, Error}}
  end.

handle(Htx, This) ->
  Method = This#this.method,
  hty_tx:dispatch(This#this.subs, hty_tx:method(Method, Htx)).
