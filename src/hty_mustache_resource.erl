-module(hty_mustache_resource).
-record(this, {}).
-export([mount/1, handle/2]).

mount(Fspath) -> {ok, #this{}}.

handle(Htx, This) -> htx_tx:ok().