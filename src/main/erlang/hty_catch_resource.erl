-module(hty_catch_resource).

-record(hty_catch_resource, {status, subs}).

-export([mount/1, new/2, handle/2]).

mount(Fspath) ->
  case lists:reverse(Fspath:parts()) of
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
    io:format("Matching status ~p~n",[Htx:status()]),
    case Htx:status() of
      {Status, _} ->
        Htx:dispatch(Subs);
      _ ->
        Htx
    end.
