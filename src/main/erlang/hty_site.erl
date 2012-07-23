%% Author: jens
%% Created: 19 jul 2012
%% Description: TODO: Add description to hty_site
-module(hty_site).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, mount/3, lookup/2]).

%%
%% API Functions
%%

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
	


