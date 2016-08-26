-module(hty_logviewer_resource).
-record(hty_logviewer_resource, {logger}).
-export([mount/1, handle/2]).

mount(Fspath) ->
  case lists:reverse(Fspath:parts()) of
    ["logviewer", BindingKey| _] ->
      {ok, #hty_logviewer_resource{logger=BindingKey}};
    _ ->
      {error, "logviewer needs a prefix to lookup bound logger"}
  end.

handle(Htx, This) ->
  From = This:first_or_default(Htx, <<"from">>, dawnoftime),
  To = This:first_or_default(Htx, <<"to">>, endofdays),
  Pattern = This:first_or_default(Htx, <<"pattern">>, ".*"),
  After = This:first_or_default(Htx, <<"after">>, 0),
  Before = This:first_or_default(Htx, <<"before">>, 0),
  Logger = Htx:bound(This#hty_logviewer_resource.logger),
  case Logger:grep(From, To, Pattern, Before, After) of
    {ok, Lines} ->
      ok(Htx, Lines);
    {gone} ->
      Htx:gone();
    {error, Error} ->
      Htx:server_error(Error);
    {timeout} ->
      Htx:service_unavailable()
  end.

ok(Htx, Lines) ->
  Htx:ok().

first_or_default(Htx, Param, Default) ->
  case Htx:queryparam(Param) of
    [] -> Default;
    [Value1|_] -> Value1
  end.
