-module(hty_fileserver).
-export([serve/2]).

serve(Htx0, Fs) ->
  ETag = [$"] ++ Fs:last_modified() ++ [$"],
  BTag = list_to_binary(ETag),
  case Htx0:req_header('If-None-Match') of
    [BTag] ->
      io:format("ETag Match: ~p~n", [ETag]),
      Htx01 = Htx0:rsp_header("ETag", ETag),
      Htx02 = Htx01:not_modified(),
      Htx02:commit();
    SomethingElse ->
      io:format("If-None-Match: ~p!=~p~n", [SomethingElse,ETag]),
      Htx01 = Htx0:rsp_header("ETag", ETag),
      Htx = Htx01:rsp_header("Cache-Control", "public"),
      Mime = Htx:mimemap(Fs:ext()),
      Htx2 = Htx:rsp_header('Content-Type', Mime),
      Htx3 = Fs:send(Htx2),
      Htx4 = Htx3:ok(),
      Htx4:commit()
  end.
