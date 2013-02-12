%% Author: jens
%% Created: 23 jan 2013
%% Description: TODO: Add description to hty_basicauth_rule
-module(hty_basicauth_rule).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([match/2]).

%%
%% API Functions
%%
match(Fspath, Rules) ->
	case lists:reverse(Fspath:parts()) of
		["basicauth"|_] ->
			Subs = lists:flatmap(fun({ok, {resource, R}, _, _}) -> [R];
														 (_) -> []
													 end, Fspath:walk(Rules)),
			{claim, {resource, hty_basicauth_resource:new(Subs)}};
		_ ->
			next
	end.


%%
%% Local Functions
%%

