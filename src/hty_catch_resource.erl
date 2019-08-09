-module(hty_catch_resource).

-record(hty_catch_resource, {status, subs}).

-export([mount/1, new/2, handle/2]).

mount(Fspath) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["catch", Status|_] ->
      case validate(Status) of
        {ok, StatusCode} ->
          {ok, Subs} = hty_mounter:walk(Fspath, "resource"),
          {ok, hty_catch_resource:new(StatusCode, Subs)};
        _ ->
          {error, {"Bad params for catch", Status}}
      end;
    _ ->
      {error, "No status param for catch"}
  end.

new(Status, Subs) ->
  #hty_catch_resource{status=Status, subs=Subs}.

validate(String) ->
  case (catch list_to_integer(String)) of
    {'EXIT', _} ->
      false;
    Int when is_integer(Int) ->
      {ok, Int}
  end.

  handle(Htx, This) ->
    Status = This#hty_catch_resource.status,
    Subs = This#hty_catch_resource.subs,
    io:format("Checking for code ~p in ~p~n",[Status, hty_tx:status(Htx)]),
    case hty_tx:status(Htx) of
      {Status, _} ->
        hty_tx:dispatch(Subs, Htx);
      _ ->
        Htx
    end.
