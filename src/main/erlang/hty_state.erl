%@doc This is a state data structure for the running hty_main loop 
-module(hty_state, [Servers, Engines, Sites]).

-export([add_server/2, add_engine/2, add_site/2]).
-export([get_server/1, get_engine/1, get_site/1]).
-export([remove_server/1, remove_engine/1, remove_site/1]).

add_server(Id, Server) -> 
    hty_state:new(store(Id, Server, Servers), Engines, Sites).

add_engine(Id, Engine) -> 
    hty_state:new(Servers, store(Id, Engine, Engines), Sites).

add_site(Id, Site) ->
    hty_state:new(Servers, Engines, store(Id, Site, Sites)).
     
get_server(Id) ->
    case lookup(Id, Servers) of
	false -> no;
	{Id, Server} -> {ok, Server}
    end.

get_engine(Id) ->
    case lookup(Id, Engines) of
	false -> no;
	{Id, Engine} -> {ok, Engine}
    end.

get_site(Id) ->
    case lookup(Id, Sites) of
	false -> no;
	{Id, Site} -> {ok, Site}
    end.
	 
remove_server(Id) -> 
    hty_state:new(drop(Id, Servers), Engines, Sites).

remove_engine(Id) -> 
    hty_state:new(Servers, drop(Id, Servers), Sites).
	
remove_site(Id) -> 
    hty_state:new(Servers, Engines, drop(Id, Sites)).
	
store(Id, Item, List) -> lists:keystore(Id, 1, List, {Id, Item}).
drop(Id, List) -> lists:keydelete(Id, 1, List).
lookup(Id, List) -> lists:keyfind(Id, 1, List).     


    


