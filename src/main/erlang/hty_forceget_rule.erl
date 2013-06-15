-module(hty_forceget_rule).

-export([match/2]).

match(Fspath, Rules) ->
    case Fspath:ext() of
	"forceget" ->
	    Subs = Fspath:subs(Rules),
	    {claim, {resource, hty_forceget_resource:new(Subs)}};
	_ ->
	    next
    end.
    
