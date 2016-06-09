-module(hty_filelog_logger).
-record(hty_filelog_logger, {actor, fspath}).
-export([mount/1, log/5]).

mount(Fspath) ->
  case spawn(fun() -> loop(Fspath) end) of
    {error, _} = Error ->
      Error;
    {ok, Actor} ->
      {ok, #hty_filelog_logger{actor=Actor, fspath=Fspath}}
  end.

log(StartTstamp, EndTstamp, Category, Message, This) ->
  Actor = This#hty_filelog_logger.actor,
  Log = {log, StartTstamp, EndTstamp, Category, Message},
  Actor ! Log.

name(Folder) ->
  Folder:subpath([hty_log:today() ++ ".log"]).

loop(Folder) ->
  receive
    {log, T0, T1, Cat, Msg} ->
      Line = [T0, $|, T1, $|, Cat, $|, Msg, 10],
      File = name(Folder),
      File:append(Line),
      loop(Folder);
    stop ->
      ok
  end.
