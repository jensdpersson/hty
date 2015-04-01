-module(hty_history_rule).

-export([match/1]).

match(Walker) ->
  Fspath = Walker:fspath(),
    case lists:reverse(Fspath:parts()) of
	["history", Type|_] ->
	    {claim, {resource, hty_history_resource:new(Fspath, Type)}};
	_ ->
	    next
    end.
