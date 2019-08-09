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
-export([mount/1, handle/2, new/1]).

mount(Fspath) ->
  {ok, [Realm]} = hty_mounter:walk(Fspath, "realm"),
  {ok, new(Realm)}.

new(Realm) ->
    #hty_realm_resource{realm=Realm}.

handle(Htx, This) ->
    Htx1 = hty_tx:realm(This#hty_realm_resource.realm, Htx),
    Htx1.
