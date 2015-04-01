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
-export([match/1]).

%%
%% API Functions
%%
match(Walker) ->
	Fspath = Walker:fspath(),
	case lists:reverse(Fspath:parts()) of
		["basicauth"|_] ->
			Subs = Walker:subs(),
			{claim, {resource, hty_basicauth_resource:new(Subs)}};
		_ ->
			next
	end.


%%
%% Local Functions
%%
