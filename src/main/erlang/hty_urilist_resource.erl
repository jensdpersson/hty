-module(hty_urilist_resource, [Fspath]).

-export([handle/1]).

handle(Htx) ->
    Fun = fun(File, Htxn) ->
        Uri = hty_uri:pack(hty_uri:append(Htx:path(), File:basename())),
        Htxn1 = Htxn:echo(Uri),
        Htxn1:echo("~r~n")
    end,
    Acc0 = Htx,
    List = Fspath:list(),
    lists:fold(Fun, Acc0, List).
