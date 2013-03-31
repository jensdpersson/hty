%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_public_rule
-module(hty_static_rule).

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
		"static" ->
			{claim, {resource, hty_static_resource:new(Fspath)}};
		_ ->
			next
	end.

%%
%% Local Functions
%%

