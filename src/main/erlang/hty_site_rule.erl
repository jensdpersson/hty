-module(hty_site_rule).

-export([match/2]).

match(Fspath, _) ->
    case Fspath:parts() of
	[Site, "site"] -> 
	    Subs = Fspath:subs([hty_rules_rule]),
	    {claim, {site, Site, hty_union_resource:new(Subs)}};
	_ -> next
    end. 
