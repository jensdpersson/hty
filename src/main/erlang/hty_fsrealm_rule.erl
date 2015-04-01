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
-export([match/1]).

%%
%% API Functions
%%
match(Walker) ->
  Fspath = Walker:fspath(),
    case lists:reverse(Fspath:parts()) of
	["fsrealm", Name | _] ->
	    Subs = Walker:subs(),
	    Realm = hty_fsrealm:new(Name, Fspath),
	    Res = hty_realm_resource:new(Realm, Subs),
	    {claim, {resource, Res}};
	_ ->
	    next
    end.


%%
%% Local Functions
%%
