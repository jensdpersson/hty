-module(hty_filelog_logger).
-record(hty_filelog_logger, {actor}).
-export([mount/1, log/5, grep/6]).

mount(Fspath) ->
  case hty_mounter:walk(Fspath, "storage") of
    {ok, [Storage]} ->
      Actor = spawn(fun() ->
        loop(hty_storage:invoke_tofs([], Storage))
      end),
      {ok, #hty_filelog_logger{actor=Actor}};
    {error, _Error} = E ->
      E
  end.

log(StartTstamp, EndTstamp, Category, Message, This) ->
  Actor = This#hty_filelog_logger.actor,
  Log = {log, StartTstamp, EndTstamp, Category, Message},
  Actor ! Log.

-spec grep(hty_date:datetime(),
          hty_date:datetime(),
          string(),
          integer(),
          integer(), #hty_filelog_logger{}) -> [
          {match, hty_date:datetime(), string(), binary()}].
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
  after 60000 ->
    {timeout}
  end.

name(Folder) ->
  Name = hty_date:format_date(hty_date:now()),
  hty_fspath:subpath([Name ++ ".log"], Folder).

loop(Folder) ->
  receive
    {log, T0, T1, Cat, Msg} ->
      Line = [T0, $|, T1, $|, Cat, $|, Msg, 10],
      File = name(Folder),
      io:format("Logger got message ~p~n", [Line]),
      case hty_fspath:append(Line, File) of
        ok -> noop;
        {error, Error} -> io:format("Failed writing to logfile ~p~n", [Error])
      end,
      %Gbtree1 = ensureLoaded(File, Gbtree),
      loop(Folder);
    stop ->
      ok;
    {grep, From, To, Pattern, Before, After, ReplyTo} ->
      case re:compile(Pattern) of
        {ok, Regex} ->
          ReplyTo ! do_grep(From, To, Regex, Before, After, Folder, []),
          loop(Folder);
        {error, Error} ->
          ReplyTo ! {error, {badregex, Error}},
          loop(Folder)
      end
  end.

do_grep(From, To, Regex, Before, After, Folder, ResultSoFar) ->
  %TODO is To before From?
  FromStr = hty_date:format_date(From),
  File = hty_fspath:subpath([FromStr ++ ".log"], Folder),
  Result = case hty_fspath:exists(File) of
    true ->
      io:format("File does exist ~p ~n",[File]),
      grep_in_file(File, Regex, Before, After, ResultSoFar);
    false ->
      io:format("File does not exist ~p ~n",[File]),
      ResultSoFar
  end,
  Tomorrow = hty_date:tomorrow(From),
  io:format("Checking tomorrow ~p~n", [Tomorrow]),
  case hty_date:format_date(To) >= hty_date:format_date(Tomorrow) of
    true ->
      io:format("Checking tomorrow ~p ... true ~n", [Tomorrow]),
      do_grep(Tomorrow, To, Regex, Before, After, Folder, Result);
    false ->
      io:format("Checking tomorrow ~p ... false ~n", [Tomorrow]),
      {ok, Result}
  end.

grep_in_file(File, Regex, _Before, _After, _Result) ->
  hty_fspath:collect(fun(Line) ->
    case re:run(Line, Regex) of
      {match, _} ->
        [Line];
      nomatch ->
        []
    end
  end, File).
