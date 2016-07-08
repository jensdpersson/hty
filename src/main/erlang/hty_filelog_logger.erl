-module(hty_filelog_logger).
-record(hty_filelog_logger, {actor, fspath}).
-export([mount/1, log/5, grep/5]).

mount(Fspath) ->
  Actor = spawn(fun() ->
    Gbtree = loadFiles(Fspath),
    loop(Fspath, Gbtree)
   end),
  {ok, #hty_filelog_logger{actor=Actor, fspath=Fspath}}.

log(StartTstamp, EndTstamp, Category, Message, This) ->
  Actor = This#hty_filelog_logger.actor,
  Log = {log, StartTstamp, EndTstamp, Category, Message},
  Actor ! Log.

loadFiles(Fspath) ->
  case Fspath:list() of
    {error, _} = E ->
      E;
    Files ->
      Gbtree = lists:foldl(fun(File, Acc) ->
        Datestring = File:basename(),
        case hty_date:parse(Datestring) of
          {error, Error} ->
            {error, Error};
          Date ->
            gb_trees:insert(Date, File, Acc)
        end
      end, gb_trees:empty(), Files),
      Gbtree
  end.

-spec grep(hty_date:datetime(),
          hty_date:datetime(),
          string(),
          integer(),
          integer(), #hty_filelog_logger) -> [{match, hty_date:datetime(), string(), binary()}].
grep(From, To, Pattern, Before, After, This) ->
  Actor = This#hty_filelog_logger.actor,
  Actor ! {grep, From, To, Pattern, Before, After, self()},
  receive
    {ok, _} = Ok ->
      Ok;
    {gone} = Gone ->
      Gone;
    {error, _} = Error ->
      Error
  after 60000
    {timeout}
  end.

name(Folder) -> Folder:subpath([hty_log:today() ++ ".log"]).

loop(Folder, Gbtree) ->
  receive
    {log, T0, T1, Cat, Msg} ->
      Line = [T0, $|, T1, $|, Cat, $|, Msg, 10],
      File = name(Folder),
      File:append(Line),
      loop(Folder);
    stop ->
      ok;
    {grep, From, To, Pattern, Before, After, ReplyTo} ->
      ReplyTo ! do_grep(From, To, Pattern, Before, After, Folder),
      loop(Folder)
  end.

  do_grep(From, To, Pattern, Before, After, Folder) ->
    case Folder:list() of
      {error, _} = Error ->
        Error;
      Files ->
        lists:

        Actor must index its files on startup and add to index on roll.

    % Find an initial file
    % read through it
    % read another file?
