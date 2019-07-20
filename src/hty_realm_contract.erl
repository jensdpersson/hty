-module(hty_realm_contract).
-record(hty_realm_contract, {realm}).

-export([handle/2, new/1]).

new(Realm) ->
  #hty_realm_contract{realm=Realm}.

handle(Htx, This) ->
  Htx1 = Htx:realm(This#hty_realm_contract.realm),
  Htx1.
