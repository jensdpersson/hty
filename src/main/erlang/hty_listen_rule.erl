-module(hty_listen_rule).

-export([match/1]).

match(Walker) ->
  Fspath = Walker:fspath(),
    case Fspath:parts() of
	[Port, "http"] ->
	    claim(Port, http, Walker);
	[Port, "https"] ->
	    claim(Port, https, Walker);
	_ -> next
    end.

claim(Port, Proto, Walker) ->
    Subs = (Walker:rules([hty_siteref_rule, hty_rules_rule])):subs(),
    Root = hty_union_resource:new(Subs),
    {claim, {listen, Proto, list_to_integer(Port), Root}}.
