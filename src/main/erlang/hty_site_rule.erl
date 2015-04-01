-module(hty_site_rule).

-export([match/1]).

match(Walker) ->
  Fspath = Walker:fspath(),
    case Fspath:parts() of
	[Site, "site"] ->
	    Subs = (Walker:rules([hty_rules_rule])):subs(),
	    {claim, {site, Site, hty_union_resource:new(Subs)}};
	_ -> next
    end.
