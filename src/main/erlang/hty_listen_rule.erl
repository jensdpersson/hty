-module(hty_listen_rule).

-export([match/2]).

match(Fspath, _Rules) ->
		case Fspath:parts() of
		     [Port, "http"] -> claim(Port, http);
		     [Port, "https"] -> claim(Port, https);
		     _ -> next
		end.

claim(Port, Type) ->
	    {claim, {listen, Type, list_to_integer(Port)}}.