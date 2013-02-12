%% Author: jens
%% Created: 28 jan 2013
%% Description: TODO: Add description to hty_xslpi_rule
-module(hty_xslpi_rule).

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
		["xslpi", Xslpath|_] ->
			Subs = lists:flatmap(fun({ok, {resource, R}, _, _}) -> [R];
												  (_) -> [] end,
											 Fspath:walk(Rules)),
			{claim, {resource, hty_xslpi_resource:new(Xslpath, Subs)}};
		_ ->
			next
	end.


%%
%% Local Functions
%%

