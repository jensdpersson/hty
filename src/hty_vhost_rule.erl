-module(hty_vhost_rule).

-export([match/1]).

match(Walker) ->
  Fspath = Walker:fspath(),
    case lists:reverse(Fspath:parts()) of
	["vhost"|VhostParts] ->
            Subs = Walker:subs(),
            Vhost = list_to_binary(string:join(lists:reverse(VhostParts), ".")),
	    {claim, {resource, hty_vhost_resource:new([Vhost], Subs)}};
	_ ->
	    next
    end.
