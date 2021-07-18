-module(hty_mustache_resource).
-record(hty_mustache_resource, {}).
-export([mount/1, handle/2]).

mount(_Fspath) -> {ok, #hty_mustache_resource{}}.

handle(_Htx, _This) -> htx_tx:ok().
