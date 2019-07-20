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
  From0 = Htx:queryparam(<<"from">>),
  From1 = case From0 of
    [] ->
      (hty_date:now()):daybreak();
    [Value|_] ->
      hty_date:parse(Value)
  end,
  To0 = Htx:queryparam(<<"to">>),
  To1 = case To0 of
    [] ->
      (hty_date:now()):nightfall();
    [Value1|_] ->
      hty_date:parse(Value1)
  end,

  case {From1, To1} of
    {{error, _Error},_} ->
      Htx1 = Htx:echo(["Bad from date:", From0]),
      Htx1:bad_request();
    {_,{error, _Error}} ->
      Htx1 = Htx:echo(["Bad to date:", To0]),
      Htx1:bad_request();
    _ ->
      Pattern = first_or_default(Htx, <<"grep">>, ".*"),
      After = first_or_default(Htx, <<"after">>, 0),
      Before = first_or_default(Htx, <<"before">>, 0),
      case Htx:bound(This#hty_logviewer_resource.logger) of
        no ->
          Htx:server_error("No logger bound as " ++ This#hty_logviewer_resource.logger);
        {ok, [Logger]} ->
          case Logger:grep(From1, To1, Pattern, Before, After) of
            {ok, Lines} ->
              ok(Htx, Lines);
            {gone} ->
              Htx:gone();
            {error, Error} ->
              Htx:server_error(Error);
            {timeout} ->
              Htx:service_unavailable()
          end
      end
  end.

ok(Htx, Lines) ->
  Htx1 = Htx:echo(Lines),
  Htx2 = Htx1:rsp_header('Content-Type', <<"text/plain">>),
  (Htx2:ok()):commit().

first_or_default(Htx, Param, Default) ->
  case Htx:queryparam(Param) of
    [] -> Default;
    [Value1|_] -> Value1
  end.
