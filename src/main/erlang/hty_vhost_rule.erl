-module(hty_vhost_rule).

-export([match/2]).

match(Fspath, Rules) ->
    case lists:reverse(Fspath:parts()) of
	["vhost"|VhostParts] ->
            Subs = Fspath:subs(Rules),
            Vhost = list_to_binary(string:join(lists:reverse(VhostParts), ".")),
	    {claim, {resource, hty_vhost_resource:new([Vhost], Subs)}};
	_ ->
	    next
    end.

