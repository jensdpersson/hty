-module(hty_site_resource).

-export([mount/2, handle/2]).

-record(hty_site_resource, {site}).

mount(Fspath, _Mc) ->
  case lists:reverse(Fspath:parts()) of
    ["site", Site | _ ] ->
      {ok, #hty_site_resource{site=Site}};
    Other ->
      {error, {unexpected, Other}}
  end.

handle(Htx, This) ->
  Siteid = This#hty_site_resource.site,
  case hty_site_server:request(Siteid, 30000) of
    {ok, Root} ->
      Htx:dispatch(Root);
    {error, timeout} ->
      Htx:service_unavailable()
  end.
