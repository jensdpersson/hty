-module(hty_site).

-record(hty_site, {name, vhosts, root}).

-export([new/3, match/2, name/1, root/1]).

new(Name, Vhosts, Root) ->
    #hty_site{name=Name,vhosts=Vhosts,root=Root}.

-spec match(Host :: atom(), This :: any()) -> {ok, any()} | false.
match(Host, This) ->
    Vhosts = This#hty_site.vhosts,
    case lists:member(Host, Vhosts) of
	true ->
	    {ok, This:root()};
	false ->
	    no
    end.

name(This) -> This#hty_site.name.
root(This) -> This#hty_site.root.
