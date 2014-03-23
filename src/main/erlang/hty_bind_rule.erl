-module(hty_bind_rule).

-export([match/2]).

match(Fspath, Rules) ->
	case lists:reverse(Fspath:parts()) of
		["bind"|_] ->
			
			Contentpath = Fspath:subpath(["content"]),
			Subs = Contentpath:subs(Rules),
			
			Bindings = Fspath:subs([hty_bindas_rule|Rules]),
			Subs1 = lists:foldl(fun(Item, Acc) -> 
									Item:next(Acc) 
							  end, Subs, Bindings),
			
			{claim, {resource, Subs1}};
		_ ->
			next
	end.