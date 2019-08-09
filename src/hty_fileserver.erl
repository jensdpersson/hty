-module(hty_fileserver).
-export([serve/2]).

serve(Htx0, Fs) ->
  ETag = [$"] ++ hty_fspath:last_modified(Fs) ++ [$"],
  BTag = list_to_binary(ETag),
  case hty_tx:req_header('If-None-Match', Htx0) of
    [BTag] ->
      io:format("ETag Match: ~p~n", [ETag]),
      Htx01 = hty_tx:rsp_header("ETag", ETag, Htx0),
      Htx02 = hty_tx:not_modified(Htx01),
      hty_tx:commit(Htx02);
    SomethingElse ->
      io:format("If-None-Match: ~p!=~p~n", [SomethingElse,ETag]),
      Htx01 = hty_tx:rsp_header("ETag", ETag, Htx0),
      Htx = hty_tx:rsp_header("Cache-Control", "public", Htx01),
      Mime = hty_tx:mimemap(hty_fspath:ext(Fs), Htx),
      Htx2 = hty_tx:rsp_header('Content-Type', Mime, Htx),
      Htx3 = hty_fspath:send(Htx2, Fs),
      Htx4 = hty_tx:ok(Htx3),
      hty_tx:commit(Htx4)
  end.
