-module(hty_ticketeer_rule).
-export([match/2]).

match(Fspath, _Rules) ->
    case Fspath:ext() of
	"ticketeer" ->
	    {claim, {resource, hty_ticketeer_resource:new(Fspath)}};
	_ ->
	    next
    end.
	    
    
