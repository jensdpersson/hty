-module(hty_vhost_rule).

-export([match/2]).

match(Fspath, _Rules) ->
    case lists:reverse(Fspath:parts()) of
	["vhost", Vhost|_] ->
	    {claim, {resource, hty_vhost_resource:new(Vhost)}};
	_ ->
	    next
    end.

