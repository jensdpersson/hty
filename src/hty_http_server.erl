-module(hty_http_server).
-export([mount/1]).

mount(Fspath) ->
  {Ip, Port} = case hty_fspath:parts(Fspath) of
    [PortSpec, "http"] ->
      {{0,0,0,0}, list_to_integer(PortSpec)};
    [A,B,C,D,PortSpec, "http"] ->
      {{list_to_integer(A),list_to_integer(B),list_to_integer(C),list_to_integer(D)}, list_to_integer(PortSpec)};   
    _ -> 
      {{0,0,0,0}, 80}
  end,
  case hty_mounter:walk(Fspath, "resource") of
    {ok, Resources} ->
      Root = hty_union_resource:new(Resources),
      {ok, hty_server:new("http", Ip, Port, Root)};
    {error, Error} ->
      {error, {?MODULE, Error}}
  end.
