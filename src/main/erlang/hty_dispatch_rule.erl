%% Author: jens
%% Created: 9 mar 2013
%% Description: TODO: Add description to hty_dispatch_resource
-module(hty_dispatch_rule).

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
		"dispatch" ->
			Subs = Walker:subs(),
			{claim, {resource, hty_union_resource:new(Subs)}};
		_ ->
			next
	end.


%%
%% Local Functions
%%
