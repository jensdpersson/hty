-module(hty_vhost_resource).

-export([mount/1, handle/2, new/2]).
-record(hty_vhost_resource, {aliases, subs}).

mount(Fspath) ->
  case lists:reverse(Fspath:parts()) of
    ["vhost", Aliases0 | _] ->
      Aliases = string:tokens(Aliases0, ","),
      case hty_mounter:walk(Fspath, "resource") of
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
  case Htx:req_header('Host') of
    [Host] ->
      case lists:member(Host, Aliases) of
        true ->
          Htx:dispatch(Subs);
        false ->
          Htx:not_found()
      end;
    [] ->
      Htx:not_found();
    _ ->
      Htx:bad_request()
  end.
