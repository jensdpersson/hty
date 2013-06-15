%@doc This is a state data structure for the running hty_main loop 
-module(hty_state, [Servers, Sites]).

-export([add_server/2, add_site/2]).
-export([get_server/1, get_site/1]).
-export([remove_server/1, remove_site/1]).
-export([sites/0, servers/0, sites/1, servers/1]).

add_server(Id, Server) -> 
    hty_state:new(store(Id, Server, Servers), Sites).

add_site(Id, Site) ->
    hty_state:new(Servers, store(Id, Site, Sites)).

sites() -> Sites.
sites(Sites1) -> 
	hty_state:new(Servers, Sites1).

servers() -> Servers.
servers(Servers1) -> 
	hty_state:new(Servers1, Sites).
     
get_server(Id) ->
    case lookup(Id, Servers) of
	false -> no;
	{Id, Server} -> {ok, Server}
    end.

get_site(Id) ->
    case lookup(Id, Sites) of
	false -> no;
	{Id, Site} -> {ok, Site}
    end.
	 
remove_server(Id) -> 
    hty_state:new(drop(Id, Servers), Sites).
	
remove_site(Id) -> 
    hty_state:new(Servers, drop(Id, Sites)).
	
store(Id, Item, List) -> lists:keystore(Id, 1, List, {Id, Item}).
drop(Id, List) -> lists:keydelete(Id, 1, List).
lookup(Id, List) -> lists:keyfind(Id, 1, List).     


    


