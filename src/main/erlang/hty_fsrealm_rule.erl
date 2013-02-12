%% Author: jens
%% Created: 4 feb 2013
%% Description: TODO: Add description to hty_userfolders_rule
-module(hty_fsrealm_rule).

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
		["fsrealm", Name | _] ->
			Subs = hty_util:subs(Fspath, Rules),
			Res = hty_realm_resource:new(Name, Subs),
			{claim, {resource, Res}};
		_ ->
			next
	end.


%%
%% Local Functions
%%

