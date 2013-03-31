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
-export([match/2]).

%%
%% API Functions
%%
match(Fspath, _Rules) ->
	case Fspath:ext() of
		"dispatch" -> 
			Subs = Fspath:subs(),
			{claim, {resource, hty_union_resource:new(Subs)}};
		_ ->
			next
	end.


%%
%% Local Functions
%%

