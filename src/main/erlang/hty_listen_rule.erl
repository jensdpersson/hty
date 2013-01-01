-module(hty_listen_rule).

-export([match/2]).

match(Fspath, _Rules) ->
    case Fspath:parts() of
		[Port, "http"] -> claim(Port, http, Fspath);
		[Port, "https"] -> claim(Port, https, Fspath);
		_ -> next
    end.

claim(Port, Proto, Fspath) ->
	case Fspath:walk([hty_siteref_rule, hty_rules_rule]) of
    	[{ok, {resource, Root}, _, _}] -> 	
    		{claim, {listen, Proto, list_to_integer(Port), Root}};
		{no, _Reason, _Path, _Rules} ->
			block
	end.
