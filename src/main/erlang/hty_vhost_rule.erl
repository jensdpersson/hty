-module(hty_vhost_rule).

-export([match/2]).

match(Fspath, Rules) ->
    case lists:reverse(Fspath:parts()) of
	["vhost", Vhost|_] ->
            Subs = Fspath:walk(Rules),
	    {claim, {resource, hty_vhost_resource:new([Vhost], Subs)}};
	_ ->
	    next
    end.

