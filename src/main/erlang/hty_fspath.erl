-module(hty_fspath).
-record(hty_fspath, {
      path,
      fs
    }).

-export([new/1, new/2]).

-export([exists/1, isdir/1, mkdir/1, last_modified/1]).
-export([match/2, walk/2, walk/3, subs/2, subs/3, list/1, list/2, parts/1, prefix/1, ext/1, subpath/2]).

-export([send/2, recv/3, load/1, save/2, append/2]).

-export([basename/1, parent/1]).

new(Path) ->
    #hty_fspath{path=Path, fs=hty_fs_fs}.

new(Path, Fs) ->
    #hty_fspath{path=Path, fs=Fs}.

path(This) ->
    This#hty_fspath.path.

exists(This) ->
  Fs = fs(This),
  Fs:exists(path(This)).

isdir(This) ->
  Fs = This#hty_fspath.fs,
  Fs:has_subs(path(This)).

mkdir(This) ->
  Fs = This#hty_fspath.fs,
  Fs:make_dir(path(This)).

last_modified(This) ->
  Fs = fs(This),
  hty_log:iso8601(Fs:last_modified(path(This))).

match(Rules, This) ->
    match(Rules, Rules, This).

match([], Allrules, This) ->
    {no, orphan, path(This), Allrules};
match([Rule|Rules], Allrules, This) ->
    Path = path(This),
    case Rule:match(This, Allrules) of
	{claim, Response} ->
	    {ok, Response, Path, Rule};
	block ->
	    {no, blocked, Path, Rule};
	{block, Why} ->
	    {no, {blocked, Why}, Path, Rule};
	next ->
	    match(Rules, Allrules, This)
    end.

list(This) ->
    Path = path(This),
    Fs = fs(This),
    {ok, Names} = Fs:list(Path),
    lists:map(fun(Name) ->
		      Path2 = filename:join([Path,Name]),
		      hty_fspath:new(Path2)
	      end, Names).

list(Filter, This) ->
    lists:filter(Filter, list(This)).

walk(Rules, This) ->
	walk(Rules, none, This).

walk(Rules, Filter, This) ->
    List = case Filter of
	       none -> list(This);
	       Filter1 -> list(Filter1, This)
	   end,
    lists:map(fun(Fscursor) ->
		      Fscursor:match(Rules)
	      end, List).

subs(Rules, This) ->
    subs(Rules, none, This).
subs(Rules, Filter, This) ->
    lists:flatmap(fun({ok, {resource, R}, _, _}) -> [R];
		     (Other) ->
			  io:format("subs:~p~n", [Other]), []
		  end, walk(Rules, Filter, This)).


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

subpath(Pathsegments, This) ->
    case lists:foldl(fun(Item, Acc) ->
			     case Item of
				 ".." -> no;
				 [$/, Item1] -> lists:reverse(Item1) ++ Acc;
				 "" -> Acc;
				 _ -> lists:reverse(Item) ++ "/" ++ Acc
			     end
		     end,
		     lists:reverse(path(This)),
		     Pathsegments) of
	no -> ascension_denied;
	Path1 -> hty_fspath:new(lists:reverse(Path1))
    end.