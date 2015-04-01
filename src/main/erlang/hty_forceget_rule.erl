-module(hty_forceget_rule).

-export([match/1]).

match(Walker) ->
  Fspath = Walker:fspath(),
    case Fspath:ext() of
	"forceget" ->
	    Subs = Walker:subs(),
	    {claim, {resource, hty_forceget_resource:new(Subs)}};
	_ ->
	    next
    end.
