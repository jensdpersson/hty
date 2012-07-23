-module(hty_site).


-export([start/1]).

start(Name) ->
    Pid = spawn(fun() -> loop(dict:)),
    Pid.

loop(State) ->
    receive
	{list, Path, ReplyTo} ->
	    list(State, Path, ReplyTo);
	{mount, Path, Resource, ReplyTo} ->
	    mount(State, Path, Resource, ReplyTo);
	{lookup, Path, ReplyTo} ->
	    lookup(Path, ReplyTo);
	{stop} -> 
	    ok
    end.
	    
	    
