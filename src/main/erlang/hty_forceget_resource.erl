-module(hty_forceget_resource,[Subs]).
-export([handle/1]).

handle(Htx) ->
    Htx1 = Htx:method('GET'),
    Htx1:dispatch(Subs).
