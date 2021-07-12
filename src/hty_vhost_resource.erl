-module(hty_vhost_resource).

-export([mount/2, handle/2, new/2]).
-record(hty_vhost_resource, {aliases, subs}).

mount(Fspath, Mc) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["vhost", Aliases0 | _] ->
      Aliases1 = string:tokens(Aliases0, ","),
      Aliases = lists:map(fun(A) -> hty_percentencoding:decode(list_to_binary(A)) end, Aliases1),
      case hty_mounter:walk(Fspath, "resource", Mc) of
        {ok, Subs} ->
          {ok, new(Aliases, Subs)};
        {error, _} = Error ->
          Error
      end;
    _ ->
      {error, "vhost resource needs a list of server names to match with incoming Host headers"}
  end.

-spec new(Aliases::list(), Subs::any()) -> #hty_vhost_resource{}.
new(Aliases, Subs) ->
  #hty_vhost_resource{aliases=Aliases, subs=Subs}.

handle(Htx, This) ->
  Aliases = This#hty_vhost_resource.aliases,
  Subs = This#hty_vhost_resource.subs,
  case hty_tx:req_header('host', Htx) of
    [Host] ->
      case lists:member(Host, Aliases) of
        true ->
          hty_tx:dispatch(Subs, Htx);
        false ->
          hty_tx:not_found(Htx)
      end;
    [] ->
      hty_tx:not_found(Htx);
    _ ->
      hty_tx:bad_request(Htx)
  end.
