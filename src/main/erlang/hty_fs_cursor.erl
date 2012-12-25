-module(hty_fs_cursor, [Path]).

-export([exists/0, match/1, walk/1, walk/2, parts/0, ext/0]).

exists() -> filelib:is_file(Path).

match(Rules) -> 
	    This = this(),	    
	    match(This, Rules, Rules).

match(_, [], Allrules) -> {no, orphan, Path, Allrules};
match(This, [Rule|Rules], Allrules) ->
		case Rule:match(This, Allrules) of
		     {claim, Response} -> 
		     		{ok, Response, Path, Rule};
		     block -> {no, blocked, Path, Rule};
		     next -> match(This, Rules, Allrules)
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

this() -> hty_fs_cursor:new(Path).

parts() ->
	Basename = filename:basename(Path),
	string:tokens(Basename, ".").

ext() ->
	[Rv|_] = lists:reverse(parts()),
	Rv.
	
