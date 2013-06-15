-module(hty_history_rule).

-export([match/2]).

match(Fspath, _Rules) ->
    case lists:reverse(Fspath:parts()) of
	["history", Type|_] ->
	    {claim, {resource, hty_history_resource:new(Fspath, Type)}};
	_ ->
	    next
    end.
