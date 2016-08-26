-module(hty_filelog_logger).
-record(hty_filelog_logger, {actor, fspath}).
-export([mount/1, log/5, grep/6]).

mount(Fspath) ->
  Actor = spawn(fun() ->
    %Gbtree = loadFiles(Fspath),
    loop(Fspath)
   end),
  {ok, #hty_filelog_logger{actor=Actor, fspath=Fspath}}.

log(StartTstamp, EndTstamp, Category, Message, This) ->
  Actor = This#hty_filelog_logger.actor,
  Log = {log, StartTstamp, EndTstamp, Category, Message},
  Actor ! Log.

%loadFiles(Fspath) ->
%  case Fspath:list() of
%    {error, _} = E ->
%      E;
%    Files ->
%      Gbtree = lists:foldl(fun(File, Acc) ->
%        ensureLoaded(File, Acc)
%      end, gb_trees:empty(), Files),
%      Gbtree
%  end.

%ensureLoaded(File, Gbtree) ->
%  case hty_date:parse(File:basename()) of
%    {error, Error} ->
%      {error, Error};
%    Date ->
%      gb_trees:insert(Date, File, Gbtree)
%  end.

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
  Name = (hty_date:now()):format_date(),
  Folder:subpath([Name ++ ".log"]).

loop(Folder) ->
  receive
    {log, T0, T1, Cat, Msg} ->
      Line = [T0, $|, T1, $|, Cat, $|, Msg, 10],
      File = name(Folder),
      io:format("Logger got message ~p~n", [Line]),
      case File:append(Line) of
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
  FromStr = From:format_date(),
  File = Folder:subpath(FromStr),
  Result = case File:exists() of
    true ->
      grep_in_file(File, Regex, Before, After, ResultSoFar);
    false ->
      ResultSoFar
  end,
  Tomorrow = From:tomorrow(),
  case To:format_date() >= Tomorrow:format_date() of
    true ->
      do_grep(Tomorrow, To, Regex, Before, After, Folder, Result);
    false ->
      Result
  end.

grep_in_file(File, Regex, Before, After, Result) ->
  File:each_line(fun(Line) ->
    case re:run(Line, Regex) of
      {match, _} ->
        [Line];
      nomatch ->
        []
    end
  end).
