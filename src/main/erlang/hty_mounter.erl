-module(hty_mounter).

-export([walk/2]).

walk(Fscursor, Rules) -> match(Fscursor, Rules, Rules).

match(Fscursor, [], Allrules) -> {no, orphan, Fscursor, Allrules};
match(Fscursor, [Rule|Rules], Allrules) ->
		case Rule:match(Fscursor, Allrules) of
		     {claim, Module} -> 
		     		{ok, Module, Fscursor, Rule};
		     block -> {no, blocked, Fscursor, Rule};
		     next -> match(Fscursor, Rules, Allrules)
		end.