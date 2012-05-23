-module(hty_site_rule).

-export([match/2]).

match(Fspath, _) ->
		case Fspath:parts() of
		     [Site, "site"] -> 
		     	    Siterules = Fspath:walk([hty_rules_rule]),
			    Fspath2 = Fspath:cd("content"),
		     	    Root = Fspath2:walk(Siterules),
		     	    {claim, {site, Site, Root}};
		     _ -> next
		end. 