-module(hty_listen_rule).

-export([match/2]).

match(Fspath, _Rules) ->
		case Fspath:parts() of
		     [Port, "http"] -> claim(Port, http, Fspath);
		     [Port, "https"] -> claim(Port, https, Fspath);
		     _ -> next
		end.

claim(Port, Type, Fspath) ->
	Root = Fspath:walk([hty_siteref_rule]), 	
	{claim, {listen, Type, list_to_integer(Port), Root}}.