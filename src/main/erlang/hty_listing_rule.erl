%% Author: jens
%% Created: 26 mar 2013
%% Description: TODO: Add description to hty_listing_rule
-module(hty_listing_rule).

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
		"listing" ->
			Basename = Fspath:basename(),
			Parentdir = Fspath:parent(),
			{claim, {resource, hty_listing_resource:new(Basename, Parentdir)}};
		_ ->
			next
	end.

%%
%% Local Functions
%%

