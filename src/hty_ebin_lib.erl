-module(hty_ebin_lib).
-record(hty_ebin_lib, {prefix}).
-export([mount/2, prefix/1]).

mount(Fspath, _Mc) ->
    case lists:reverse(hty_fspath:parts(Fspath)) of
        ["ebin", Prefix|_] ->
            Path = hty_fspath:path(Fspath),
            code:add_patha(Path),
            {ok, #hty_ebin_lib{prefix=Prefix}};
        ["ebin"] ->
            {error, "ebin_lib needs a prefix parameter"}
    end.
    
prefix(This) -> This#hty_ebin_lib.prefix.
