%% @author jens
%% @doc @todo Add description to hty_aggregate_rule.


-module(hty_aggregate_rule).

%% ====================================================================
%% API functions
%% ====================================================================
-export([match/1]).

match(Walker) ->
	Fspath = Walker:fspath(),
	case lists:reverse(Fspath:parts()) of
		["aggregate", RootElm|_] ->
			Subs = Walker:subs(),
			{claim, {resource, {hty_aggregate_resource, RootElm, Subs}}};
		_ ->
			next
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
