-module(hty_siteref_resource).

-export([mount/2, handle/2]).

-record(hty_siteref_resource, {site}).

mount(Fspath, _Mc) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["siteref", Site | _ ] ->
      {ok, #hty_siteref_resource{site=Site}};
    Other ->
      {error, {unexpected, Other}}
  end.

handle(Htx, This) ->
  Siteid = This#hty_siteref_resource.site,
  case hty_site_server:request(Siteid, 30000) of
    {ok, Root} ->
      hty_tx:dispatch(Root, Htx);
    {error, timeout} ->
      hty_tx:service_unavailable(Htx)
  end.
