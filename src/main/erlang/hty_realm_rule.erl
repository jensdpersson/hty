%% Author: jens
%% Created: 22 jan 2013
%% Description: TODO: Add description to hty_realm_rule
-module(hty_realm_rule).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([match/1]).
%%
%% API Functions
%%
match(Walker) ->
	Fspath = Walker:fspath(),
	case lists:reverse(Fspath:parts()) of
		["realm", Module|_] ->
			Subs = lists:flatmap(fun({ok, {resource, R}, _, _}) -> [R];
														 (_) -> []
													 end, Walker:walk()),
			{claim, {resource, hty_realm_resource:new(list_to_atom(Module), Subs)}};
		_ ->
			next
	end.


%%
%% Local Functions
%%
