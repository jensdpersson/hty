%% @author jens
%% @doc @todo Add description to hty_bindas_rule.


-module(hty_bindas_rule).

%% ====================================================================
%% API functions
%% ====================================================================
-export([match/2]).


match(Fspath, Rules) ->
	case lists:reverse(Fspath:parts()) of
		["bind-as", Key|_] ->
			Value = Fspath:subs(Rules),
			{claim, {resource, hty_bind_resource:new(Key, Value, [])}};
		_ ->
			next
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


