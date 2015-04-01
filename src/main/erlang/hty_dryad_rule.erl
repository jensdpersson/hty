%% @author jens
%% @doc @todo Add description to hty_dryad_rule.


-module(hty_dryad_rule).

%% ====================================================================
%% API functions
%% ====================================================================
-export([match/1]).

match(Walker) ->
	Fspath = Walker:fspath(),
	case lists:reverse(Fspath:parts()) of
		["dryad", Proplist|_] ->
			Props = hty_util:parse_key_value_list(Proplist),
			{"storage", Storage} = lists:keyfind("storage", 1, Props),
			{"taxonomy", Taxonomy} = lists:keyfind("taxonomy", 1, Props),
			{"roottype", Roottype} = lists:keyfind("roottype", 1, Props),
			Resource = hty_dryad_resource:new(Storage,
											  Taxonomy,
											  Roottype),
			{claim, {resource, Resource}};
		_ ->
			next
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
