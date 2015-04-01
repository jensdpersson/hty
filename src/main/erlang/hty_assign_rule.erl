%% @author jens
%% @doc @todo Add description to hty_assign_rule.


-module(hty_assign_rule).

%% ====================================================================
%% API functions
%% ====================================================================
-export([match/1]).


match(Walker) ->
	Fspath = Walker:fspath(),
	case lists:reverse(Fspath:parts()) of
		["assign", Assignments|_] ->
			Asses = hty_util:parse_key_value_list(Assignments),
			Subs = Walker:subs(),
			{claim, {resource, hty_assign_resource:new(Asses, Subs)}};
		_ ->
			next
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
