-module(hty_accesslog_resource).
-record(hty_accesslog_resource, {key, subs}).
-export([mount/2, new/2, handle/2]).


mount(Fspath, Mc) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["accesslog", Key | _] ->
      case hty_mounter:walk(Fspath, "resource", Mc) of
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
    io:format("AccessLog.Handle"),
    Key = This#hty_accesslog_resource.key,
    Subs = This#hty_accesslog_resource.subs,
    {ok, Loggers} = hty_tx:bound(Key, Htx),

    %Initial values
    Method = atom_to_list(hty_tx:method(Htx)),
    Path = hty_uri:pack(hty_tx:path(Htx)),
    T0 = hty_date:format(hty_date:now()),


    %Dispatch
    Htx1 = hty_tx:dispatch(Subs, Htx),

    %More stuff
    {StatusCode, StatusText} = hty_tx:status(Htx1),
    T1 = hty_date:format(hty_date:now()),

    Category = [Method, " ", Path],
    Peer = case hty_tx:peer(Htx1) of
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
      hty_filelog_logger:log(T0, T1, Category, Message, Logger)
    end, Loggers),
    Htx1.

s(Int) -> integer_to_list(Int).
