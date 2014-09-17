-module(hty_fs_cursor).
-record(hty_fs_cursor, {path}).

-export([new/1]).

-export([exists/1, isdir/1, mkdir/1, last_modified/1]).
-export([match/2, walk/2, walk/3, subs/2, subs/3, list/1, list/2, parts/1, prefix/1, ext/1, subpath/2]).

-export([filepath/1, basename/1, parent/1]).

new(Path) ->
    {hty_fs_cursor, Path}.

path(This) ->
    This#hty_fs_cursor.path.

exists(This) -> 
    filelib:is_file(path(This)).
isdir(This) -> filelib:is_dir(path(This)).
mkdir(This) -> file:make_dir(path(This)).
last_modified(This) -> hty_log:iso8601(filelib:last_modified(path(This))).

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
    {ok, Names} = file:list_dir(Path),
    lists:map(fun(Name) -> 
		      Path2 = filename:join([Path,Name]),
		      hty_fs_cursor:new(Path2)
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
	hty_fs_cursor:new(P).

filepath(This) -> path(This).
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
	Path1 -> hty_fs_cursor:new(lists:reverse(Path1))
    end.
