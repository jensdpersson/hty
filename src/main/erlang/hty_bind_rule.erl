-module(hty_bind_rule).

-export([match/2]).

match(Fspath, Rules) ->
	case lists:reverse(Fspath:parts()) of
		["bind"|_] ->
			io:format("BindClaim~n"),
			Contentpath = Fspath:subpath(["content"]),
			Subs = Contentpath:subs(Rules),
			
			Filter = fun(Fspath1) -> 
							 %Fspath1 = hty_fs_cursor:new(File),
							 Fspath1:basename() /= "content"
					 end,
			
			Bindings = Fspath:subs([hty_bindas_rule|Rules], Filter),
			Subs1 = lists:foldl(fun(Item, Acc) -> 
									Item:next(Acc) 
							  end, Subs, Bindings),
			
			{claim, {resource, Subs1}};
		_ ->
			next
	end.