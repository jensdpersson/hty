-module(hty_logviewer_resource).
-record(hty_logviewer_resource, {logger}).
-export([mount/2, handle/2]).

mount(Fspath, _Mc) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["logviewer", BindingKey| _] ->
      {ok, #hty_logviewer_resource{logger=BindingKey}};
    _ ->
      {error, "logviewer needs a prefix to lookup bound logger"}
  end.

handle(Htx, This) ->
  From0 = hty_tx:queryparam(<<"from">>, Htx),
  From1 = case From0 of
    [] ->
      hty_date:daybreak(hty_date:now());
    [Value|_] ->
      hty_date:parse(Value)
  end,
  To0 = hty_tx:queryparam(<<"to">>, Htx),
  To1 = case To0 of
    [] ->
      hty_date:nightfall(hty_date:now());
    [Value1|_] ->
      hty_date:parse(Value1)
  end,

  case {From1, To1} of
    {{error, _Error},_} ->
      Htx1 = hty_tx:echo(["Bad from date:", From0], Htx),
      hty_tx:bad_request(Htx1);
    {_,{error, _Error}} ->
      Htx1 = hty_tx:echo(["Bad to date:", To0], Htx),
      hty_tx:bad_request(Htx1);
    _ ->
      Pattern = first_or_default(Htx, <<"grep">>, ".*"),
      After = first_or_default(Htx, <<"after">>, 0),
      Before = first_or_default(Htx, <<"before">>, 0),
      case hty_tx:bound(This#hty_logviewer_resource.logger, Htx) of
        no ->
          hty_tx:server_error("No logger bound as " ++ This#hty_logviewer_resource.logger, Htx);
        {ok, [Logger]} ->
          case hty_logger:invoke_grep(From1, To1, Pattern, Before, After, Logger) of
            {ok, Lines} ->
              ok(Htx, Lines);
            {gone} ->
              hty_tx:gone(Htx);
            {error, Error} ->
              hty_tx:server_error(Error, Htx);
            {timeout} ->
              hty_tx:service_unavailable(Htx)
          end
      end
  end.

ok(Htx, Lines) ->
  Htx1 = hty_tx:echo(Lines, Htx),
  Htx2 = hty_tx:rsp_header('Content-Type', <<"text/plain">>, Htx1),
  hty_tx:commit(hty_tx:ok(Htx2)).

first_or_default(Htx, Param, Default) ->
  case hty_tx:queryparam(Param, Htx) of
    [] -> Default;
    [Value1|_] -> Value1
  end.
