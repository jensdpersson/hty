%% @author jens
%% @doc @todo Add description to hty_assign_rule.


-module(hty_assign_rule).

%% ====================================================================
%% API functions
%% ====================================================================
-export([match/2]).


match(Fspath, _Rules) ->
	case lists:reverse(Fspath:parts()) of
		["assign", Assignments|_] ->
			Asses = hty_util:parse_key_value_list(Assignments),
			Subs = Fspath:subs(),
			{claim, {resource, hty_assign_resource:new(Asses, Subs)}};
		_ ->
			next
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


