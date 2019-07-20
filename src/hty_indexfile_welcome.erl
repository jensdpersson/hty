-module(hty_indexfile_welcome).
-record(hty_indexfile_welcome, {indexfiles}).
-export([new/0, new/1, list/3]).

new() ->
  #hty_indexfile_welcome{indexfiles=["index.html", "index.xml", "index.txt"]}.

new(Indexfiles) ->
  #hty_indexfile_welcome{indexfiles=Indexfiles}.

list(Htx, Fspath1, This) ->
  F=fun(W) ->
    Fspath2 = Fspath1:subpath([W]),
    case Fspath2:exists() of
      true -> [Fspath2];
      false -> []
    end
  end,
  L = This#hty_indexfile_welcome.indexfiles,
  case lists:flatmap(F, L) of
    [Indexfile|_] ->
      hty_fileserver:serve(Htx, Indexfile);
    [] ->
      Htx:not_found()
  end.
