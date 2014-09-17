-module(hty_forceget_resource).
-export([handle/2, new/1]).
-record(hty_forceget_resource, {subs}).

new(Subs) ->
    #hty_forceget_resource{subs=Subs}.

handle(Htx, This) ->
    Htx1 = Htx:method('GET'),
    Htx1:dispatch(This#hty_forceget_resource.subs).
