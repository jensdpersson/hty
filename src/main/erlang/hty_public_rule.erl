%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_public_rule
-module(hty_public_rule).

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

match(Fspath, _Rules) ->
	case Fspath:ext() of
		"public" ->
			{claim, {resource, hty_public_resource:new(Fspath)}};
		_ ->
			next
	end.

%%
%% Local Functions
%%

