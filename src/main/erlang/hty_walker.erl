-module(hty_walker, [Rules]).

-exports([walk/2]).

%@doc MatchResults are returned by Rule:match and consist of a triple Claim, Resource, Subs. Claim is one of the atoms none, fallback, exclusive and inclusive. None means no claim is made. Fallback means use this resource if no one else registers a claim, exclusive means deny all other claims and inclusive means use this claim but dont deny others. If there is no claim there is an error. If there are two exclusive claims there is an error. 
walk(Path) ->
	Apply = fun(Rule, {Score, Resource, Recurse}) ->
			{none, [], no}
			end,   
	lists:foldl(Apply, {0, []}, Rules). 


		  

	
