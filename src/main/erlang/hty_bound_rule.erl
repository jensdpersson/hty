%% @author jens
%% @doc @todo Add description to hty_bound_rule.


-module(hty_bound_rule).

%% ====================================================================
%% API functions
%% ====================================================================
-export([match/2]).

match(Fspath, _Rules) ->
	case lists:reverse(Fspath:parts()) of
		["bound", Key|_] ->
			{claim, {resource, hty_bound_resource:new(Key)}};
		_ ->
			next
	end.
%% ====================================================================
%% Internal functions
%% ====================================================================


