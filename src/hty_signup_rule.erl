%% Author: jens
%% Created: 18 feb 2013
%% Description: TODO: Add description to hty_signup_rule
-module(hty_signup_rule).

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
	case Fspath:ext() of
		"signup" ->
			{claim, {resource, hty_signup_resource}};
		_ ->
			next
	end.

%%
%% Local Functions
%%
