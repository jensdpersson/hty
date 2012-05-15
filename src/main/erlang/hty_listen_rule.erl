-module(hty_listen_rule).

-export([match/2]).

match(Fscursor, Rules) ->
		case Fscursor:parts() of
		     [Port, http] when is_integer(Port) ->
		     	    {claim, };