-module(hty_bind_rule).

-export([match/2]).

match(Fspath, Rules) ->
	case list:reverse(Fspath:parts()) of
		["bind", Var|_] ->
			Bound = Fspath:subpath([""]),
			Subs = Fspath:subs(Rules),
			Resource = hty_bind_resource:new({Var,Bound}, Subs),
			{claim, {resource, Resource}};
		_ ->
			next
	end.