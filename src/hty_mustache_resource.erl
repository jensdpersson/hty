-module(hty_mustache_resource).
-record(hty_mustache_resource, {}).
-export([mount/1, handle/2]).

mount(Fspath) -> {ok, #hty_mustache_resource{}}.

handle(Htx, This) -> htx_tx:ok().
