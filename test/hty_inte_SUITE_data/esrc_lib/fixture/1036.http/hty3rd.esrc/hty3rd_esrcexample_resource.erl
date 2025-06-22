-module(hty3rd_esrcexample_resource).
-record(hty3rd_esrcexample_resource, {}).

-export([mount/2, handle/1]).

mount(Fspath, Mc) ->
    case lists:reverse(hty_fspath:parts(Fspath)) of
        ["hty3rd"|_] -> 
            {ok, #hty3rd_esrcexample_resource{}};
        Other -> 
            {error, Error}
    end.
    
handle(Htx, _This) ->
    hty_tx:with([
        {echo, "Hello thirdparty!"},
        ok,
        commit
        ], Htx).