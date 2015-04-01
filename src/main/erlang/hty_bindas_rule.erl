%% @author jens
%% @doc @todo Add description to hty_bindas_rule.


-module(hty_bindas_rule).

%% ====================================================================
%% API functions
%% ====================================================================
-export([match/1]).


match(Walker) ->
	Fspath = Walker:fspath(),
	case Fspath:parts() of
		["bind-as", Key] ->
			Value = Walker:subs(),
			{claim, {resource, hty_bind_resource:new(Key, Value, [])}};
		_ ->
			next
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
