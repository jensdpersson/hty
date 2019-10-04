-module(hty_http_server).
-export([mount/1]).

mount(Fspath) ->
  Port = case hty_fspath:parts(Fspath) of
    [PortSpec, "http"] ->
      list_to_integer(PortSpec);
    _ -> 80
  end,
  case hty_mounter:walk(Fspath, "resource") of
    {ok, Resources} ->
      Root = hty_union_resource:new(Resources),
      {ok, hty_server:new("http", {0,0,0,0}, Port, Root)};
    {error, Error} ->
      {error, {?MODULE, Error}}
  end.