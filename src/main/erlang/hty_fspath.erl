-module(hty_fspath).
-record(hty_fspath, {
      path,
      fs
    }).

-export([new/1, new/2]).

-export([exists/1, isdir/1, mkdir/1, last_modified/1]).
-export([list/1, list/2, parts/1, prefix/1, ext/1, subpath/2]).

-export([send/2, recv/3, load/1, save/2, append/2, path/1]).

-export([basename/1, parent/1, type/1]).

-export([collect/2, params/1, param/2]).

new(Path) ->
    #hty_fspath{path=Path, fs=hty_fs_fs}.

new(Path, Fs) ->
    #hty_fspath{path=Path, fs=Fs}.

path(This) ->
    This#hty_fspath.path.

params(This) ->
  io:format("Parts=~w\n", [parts(This)]),
  case parts(This) of
    [_, Paramstring, _] ->
      Paramstrings = string:tokens(Paramstring, ","),
      lists:map(fun(Elem) ->
        case string:tokens(Elem, "=") of
          [Key, Value] ->
            {Key, Value};
          [Key] ->
            {Key, []}
        end
      end, Paramstrings);
    _ ->
      []
  end.

param(Name, This) ->
  io:format("Name=~p, Params=~w\n", [Name, This:params()]),
  case lists:keyfind(Name, 1, This:params()) of
    false ->
      no;
    {_, Value} ->
      Value
  end.

exists(This) ->
  Fs = fs(This),
  Fs:exists(path(This)).

isdir(This) ->
  Fs = This#hty_fspath.fs,
  Fs:has_subs(path(This)).

mkdir(This) ->
  Fs = This#hty_fspath.fs,
  Fs:mkdir(path(This)).

last_modified(This) ->
  Fs = fs(This),
  hty_log:iso8601(Fs:last_modified(path(This))).


-spec list(#hty_fspath{}) -> {error, any()} | [#hty_fspath{}].
list(This) ->
    Path = path(This),
    Fs = fs(This),
    case Fs:list(Path) of
      {ok, Names} ->
        lists:map(fun(Name) ->
		      Path2 = filename:join([Path,Name]),
		      hty_fspath:new(Path2)
	      end, Names);
      {error, enoent} ->
        {error, {enoent, Path}}
    end.

list(Filter, This) ->
    lists:filter(Filter, list(This)).

fs(This) ->
  This#hty_fspath.fs.

parts(This) ->
	string:tokens(basename(This), ".").



ext(This) ->
	[Rv|_] = lists:reverse(parts(This)),
	Rv.

prefix(This) ->
	[A|_] = parts(This),
	A.

parent(This) ->
	Path = path(This),
	P = filename:dirname(Path),
	hty_fspath:new(P).

save(Data, This) ->
  (fs(This)):save(path(This), Data).

load(This) ->
  (fs(This)):load(path(This)).

send(Htx, This) ->
  (fs(This)):send(path(This), Htx).

recv(Spafs, Htx, This) ->
  (fs(This)):recv(path(This), Spafs, Htx).

append(Data, This) ->
  (fs(This)):append(path(This), Data).

basename(This) -> filename:basename(path(This)).

type(This) ->
  (fs(This)):type(path(This)).

subpath(Pathsegments, This) ->
  Fun = fun(Item, Acc) ->
    case Acc of
      no -> no;
      _ ->
        case Item of
          ".." -> no;
          [$/, Item1] -> lists:reverse(Item1) ++ Acc;
          "" -> Acc;
          _ -> lists:reverse(Item) ++ "/" ++ Acc
        end
    end
  end,
  RPath = lists:reverse(path(This)),
  case lists:foldl(Fun, RPath, Pathsegments) of
    no -> ascension_denied;
    Path1 -> hty_fspath:new(lists:reverse(Path1))
  end.

collect(Predicate, This) ->
  (fs(This)):collect(path(This), Predicate).
