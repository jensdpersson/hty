%% @author jens
%% @doc @todo Add description to hty_aggregate_rule.


-module(hty_aggregate_rule).

%% ====================================================================
%% API functions
%% ====================================================================
-export([match/2]).

match(Fspath, Rules) ->
	case lists:reverse(Fspath:parts()) of
		["aggregate", RootElm|_] ->
			Subs = Fspath:subs(Rules),
			{claim, {resource, {hty_aggregate_resource, RootElm, Subs}}};
		_ ->
			next
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


