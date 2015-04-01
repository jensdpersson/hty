-module(hty_ticketeer_rule).
-export([match/1]).

match(Walker) ->
  Fspath = Walker:fspath(),
    case Fspath:ext() of
	"ticketeer" ->
	    {claim, {resource, hty_ticketeer_resource:new(Fspath)}};
	_ ->
	    next
    end.
