-module(hty_esrc_lib).
-record(hty_esrc_lib, {prefix}).

-export([mount/2, prefix/1]).

mount(Fspath, _Mc) ->
    case lists:reverse(hty_fspath:parts(Fspath)) of
        [Prefix, "esrc"] ->
            Files = hty_fspath:list(Fspath),
            Outdir = hty_fspath:path(Fspath),
            lists:foreach(fun(F) -> compile:file(F, [{outdir, Outdir}]) end, Files),
            code:add_patha(Outdir),
            {ok, #hty_esrc_lib{prefix=Prefix}};
        Other -> 
            {error, Other}
    end.

prefix(This) ->
    This#hty_esrc_lib.prefix. 
