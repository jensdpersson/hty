<<<<<<< local
%% Author: jens
%% Created: 19 jul 2012
%% Description: TODO: Add description to hty_site
=======
>>>>>>> other
-module(hty_site).

<<<<<<< local
%%
%% Include files
%%
=======
>>>>>>> other

<<<<<<< local
%%
%% Exported Functions
%%
-export([start/0, mount/3, lookup/2]).
=======
-export([start/1]).
>>>>>>> other

<<<<<<< local
%%
%% API Functions
%%
=======
start(Name) ->
    Pid = spawn(fun() -> loop(dict:)),
    Pid.
>>>>>>> other

<<<<<<< local
start() ->
	spawn(fun() -> loop([]) end).

lookup(Sitepid, Path) ->
	Sitepid ! {follow, Path},
	receive
		{path, Res} -> {ok, Res}
	after 
			1000 -> {no, timeout}
	end.
		
mount(Sitepid, Path, Resource) ->
	no.

%%
%% Local Functions
%%
loop(Tree) ->
	receive
		{follow, Path, ReplyTo} -> 
			Res = follow(Tree, Path),
			ReplyTo ! {path, Res},
			loop(Tree)
	end.

follow([], []) -> no.
	


=======
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
	    
	    
>>>>>>> other
