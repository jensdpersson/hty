%@doc This is a state data structure for the running hty_main loop 
-module(hty_state).

-export([new/2,  add_server/3, add_site/3]).
-export([get_server/2, get_site/2]).
-export([remove_server/2, remove_site/2]).
-export([sites/1, servers/1, sites/2, servers/2]).

-record(hty_state, {servers, sites}).

new(Servers, Sites) ->
    #hty_state{servers=Servers, sites=Sites}.

add_server(Id, Server, This) -> 
    This#hty_state{servers=store(Id, Server, This#hty_state.servers), sites=This#hty_state.sites}.

add_site(Id, Site, This) ->
    This#hty_state{servers=This#hty_state.servers, sites=store(Id, Site, This#hty_state.sites)}.

sites(This) ->
    This#hty_state.sites.

sites(Sites1, This) -> 
    This#hty_state{servers=This#hty_state.servers, sites=Sites1}.

servers(This) ->
    This#hty_state.servers.

servers(Servers1, This) -> 
    This#hty_state{servers=Servers1, sites=This#hty_state.sites}.
     
get_server(Id, This) ->
    case lookup(Id, This#hty_state.servers) of
	false ->
	    no;
	{Id, Server} ->
	    {ok, Server}
    end.

get_site(Id, This) ->
    case lookup(Id, This#hty_state.sites) of
	false -> no;
	{Id, Site} -> {ok, Site}
    end.
	 
remove_server(Id, This) -> 
    This#hty_state{servers=drop(Id, This#hty_state.servers), sites=This#hty_state.sites}.
	
remove_site(Id, This) -> 
    This#hty_state{sites=drop(Id, This#hty_state.sites)}.
	
store(Id, Item, List) -> 
    lists:keystore(Id, 1, List, {Id, Item}).

drop(Id, List) ->
    lists:keydelete(Id, 1, List).

lookup(Id, List) ->
    lists:keyfind(Id, 1, List).     


    


