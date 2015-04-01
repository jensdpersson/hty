-module(hty_bind_rule).

-export([match/1]).

match(Walker) ->
	Fspath = Walker:fspath(),
	case lists:reverse(Fspath:parts()) of
		["bind"|_] ->
			io:format("BindClaim~n"),
			Contentpath = Fspath:subpath(["content"]),
			Subs = (Walker:fspath(Contentpath)):subs(),

			Filter = fun(Fspath1) ->
							 Fspath1:basename() /= "content"
					 end,

			BindingsWalker = Walker:rules([hty_bindas_rule|Walker:rules()]),
			Bindings = BindingsWalker:subs(Filter),
			Subs1 = lists:foldl(fun(Item, Acc) ->
									Item:next(Acc)
							  end, Subs, Bindings),

			{claim, {resource, Subs1}};
		_ ->
			next
	end.
