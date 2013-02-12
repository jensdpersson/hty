%% Author: jens
%% Created: 22 jan 2013
%% Description: TODO: Add description to hty_realm_resource
-module(hty_realm_resource, [Realm, Subs]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/1]).

%%
%% API Functions
%%
handle(Htx) ->
	Htx1 = Htx:realm(Realm),
	Htx1:dispatch(Subs).


%%
%% Local Functions
%%

