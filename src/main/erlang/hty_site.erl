-module(hty_site, [Name, Vhosts, Root]).

-export([match/1, name/0, root/0]).

-spec match(Host :: atom()) -> {ok, any()} | false.
match(Host) ->
    case lists:member(Host, Vhosts) of
	true ->
	    {ok, Root};
	false ->
	    no
    end.

name() -> Name.
root() -> Root.
