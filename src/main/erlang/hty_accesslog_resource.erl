-module(hty_accesslog_resource).
-record(hty_accesslog_resource, {key, subs}).
-export([mount/1, new/2, handle/2]).

mount(Fspath) ->
  case lists:reverse(Fspath:parts()) of
    ["accesslog", Key | _] ->
      case hty_mounter:walk(Fspath, "resource") of
        {ok, Subs} ->
          {ok, new(Key, Subs)};
        {error, _} = Error ->
          Error
      end;
    _ ->
      {error, "accesslog needs a bound logger"}
  end.

new(Key, Subs) ->
  #hty_accesslog_resource{key=Key, subs=Subs}.

-spec handle(hty_tx:htx(), #hty_accesslog_resource{}) -> hty_tx:htx().
handle(Htx, This) ->
    Key = This#hty_accesslog_resource.key,
    Subs = This#hty_accesslog_resource.subs,
    {ok, Loggers} = Htx:bound(Key),

    %Initial values
    Method = atom_to_list(Htx:method()),
    Path = hty_uri:pack(Htx:path()),
    T0 = (hty_date:now()):format(),

    %Dispatch
    Htx1 = Htx:dispatch(Subs),

    %More stuff
    {StatusCode, StatusText} = Htx1:status(),
    T1 = (hty_date:now()):format(),

    Category = [Method, " ", Path],
    Peer = case Htx1:peer() of
    	{ipv4, {A,B,C,D}, Port} ->
    	  [" (", s(A), $., s(B), $., s(C), $., s(D), $:, s(Port), $)];
    	{ipv6, {A,B,C,D,E,F,G,H}, Port} ->
    	  [" (", s(A), $., s(B), $., s(C), $., s(D), $., s(E), $., s(F), $., s(G), $., s(H), $:,
          s(Port), $)];
    	_ -> ""
    end,
    Message = [Peer, " ", integer_to_list(StatusCode), " ", StatusText],
    lists:foreach(fun(Logger) ->
      io:format("Sending to logger ~p", [Logger]),
      Logger:log(T0, T1, Category, Message)
    end, Loggers),
    Htx1.

s(Int) -> integer_to_list(Int).
