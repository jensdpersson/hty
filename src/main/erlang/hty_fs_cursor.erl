-module(hty_fs_cursor, [Path]).

-export([exists/0, isdir/0, mkdir/0]).
-export([match/1, walk/1, walk/2, subs/1, list/0, list/1, parts/0, prefix/0, ext/0, subpath/1]).

-export([filepath/0, basename/0, parent/0]).


exists() -> filelib:is_file(Path).
isdir() -> filelib:is_dir(Path).
mkdir() -> file:make_dir(Path).

match(Rules) -> 
	    This = this(),	    
	    match(This, Rules, Rules).

match(_, [], Allrules) -> {no, orphan, Path, Allrules};
match(This, [Rule|Rules], Allrules) ->
	%io:format("Matching ~p with ~p gives", [Rule, Path]),
	case Rule:match(This, Allrules) of
		{claim, Response} -> 
			%io:format(" ~p~n", [claim]),
			{ok, Response, Path, Rule};
		block ->
			%io:format(" ~p~n", [block]),
			{no, blocked, Path, Rule};
		{block, Why} -> 
			%io:format(" ~p~n", [block]),
			{no, {blocked, Why}, Path, Rule};
		next -> 
			%io:format(" ~p~n", [next]),
			match(This, Rules, Allrules)
	end.

list() ->
	{ok, Names} = file:list_dir(Path),
	lists:map(fun(Name) -> 
					  Path2 = filename:join([Path,Name]),
					  hty_fs_cursor:new(Path2)
			  end, Names).

list(Filter) ->
	lists:filter(Filter, list()).

walk(Rules) ->
	walk(Rules, none).

walk(Rules, Filter) ->
	List = case Filter of
			   none -> list();
			   Filter1 -> list(Filter1)
		   end,
	lists:map(fun(Fscursor) -> 
					  Fscursor:match(Rules)
			  end, List).

subs(Rules) ->
    lists:flatmap(fun({ok, {resource, R}, _, _}) -> [R];
		     (Other) -> io:format("subs:~p~n", [Other]), []
		  end, walk(Rules)).


this() -> hty_fs_cursor:new(Path).

parts() -> 
	string:tokens(basename(), ".").

ext() ->
	[Rv|_] = lists:reverse(parts()),
	Rv.

prefix() ->
	[A|_] = parts(),
	A.

parent() ->
	P = filename:dirname(Path),
	hty_fs_cursor:new(P).

filepath() -> Path.
basename() -> filename:basename(Path).
	
subpath(Pathsegments) ->
	case lists:foldl(fun(Item, Acc) -> 
						case Item of
							".." -> no;
							[$/, Item1] -> lists:reverse(Item1) ++ Acc;
							"" -> Acc;
							_ -> lists:reverse(Item) ++ "/" ++ Acc
						end
				end,
				lists:reverse(Path),
				Pathsegments) of
		no -> ascension_denied;
		Path1 -> hty_fs_cursor:new(lists:reverse(Path1))
	end.
