-module(hty_site_rule).

-export([match/2]).

match(Fspath, _) ->
	case Fspath:parts() of
		[Site, "site"] -> 
			Walked = Fspath:walk([hty_rules_rule]),
			io:format("Walked [~p]~n", [Walked]),
			Subs = lists:flatmap(fun({ok, {resource, R},_ ,_}) ->
																[R];
														 (_) -> [] end,
													 Walked),
			{claim, {site, Site, hty_union_resource:new(Subs)}};
		_ -> next
	end. 
