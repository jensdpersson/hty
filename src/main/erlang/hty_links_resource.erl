-module(hty_links_resource, [Fspath]).
-export([handle/1]).

handle(Htx) ->
    Htx:server_error("Not implemented").


