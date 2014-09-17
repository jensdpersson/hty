%% Author: jens
%% Created: 22 jan 2013
%% Description: TODO: Add description to hty_realm_resource
-module(hty_realm_resource).
-record(hty_realm_resource, {realm, subs}).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/2]).

%%
%% API Functions
%%
new(Realm, Subs) ->
    #hty_realm_resource{realm=Realm,subs=Subs}.

handle(Htx, This) ->
    Htx1 = Htx:realm(This#hty_realm_resource.realm),
    Htx1:dispatch(This#hty_realm_resource.subs).


%%
%% Local Functions
%%

