-module(hty_site_server).
-record(hty_site_server, {regname, subs}).

-export([mount/2, start/1, stop/1]).
-export([request/2]).

mount(Fspath, Mc) ->
  case hty_fspath:parts(Fspath) of
    [Site, "site"] ->
      case hty_mounter:walk(Fspath, "resource", Mc) of
        {ok, Subs} ->
          {ok, #hty_site_server{regname=regname(Site),subs=Subs}};
        {error, Error} ->
          {error, Error}
      end;
    _ ->
      {error, "hty_site_server needs a site name parameter"}
  end.

loop(Resource) ->
  receive
    {stop, ReplyTo} ->
      ReplyTo ! ok;
    {request, ReplyTo} ->
      ReplyTo ! {ok, Resource},
      loop(Resource)
  end.

start(This) ->
  Actor = spawn(fun() -> loop(This#hty_site_server.subs) end),
  RegName = This#hty_site_server.regname,
  register(RegName, Actor),
  {ok, This}.

stop(This) ->
  RegName = This#hty_site_server.regname,
  RegName ! stop,
  receive
    ok ->
      ok;
    Other ->
      {error, {unexpected, Other}}
  end.

regname(SiteName) ->
  list_to_atom("hty_site_" ++ SiteName).

request(SiteName, Timeout) ->
  regname(SiteName) ! {request, self()},
  receive
    {ok, Resource} ->
      {ok, Resource}
  after
    Timeout ->
      {error, timeout}
  end.
