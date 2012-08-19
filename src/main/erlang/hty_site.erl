-module(hty_site, [Siteid, Sitepid]).

%%
%% Include files
%%
%%
%% Exported Functions
%%
-export([mount/2, lookup/1]).

%%
%% API Functions
%%
lookup(Path) ->
	Sitepid ! {lookup, Path, self()},
	receive
		{ok, Res} -> {ok, Res};
		{no, Path} -> {no, novalue}
	after 
			1000 -> {no, timeout}
	end.
		
mount(Path, Resource) ->
	Sitepid ! {mount, Path, Resource, self()},
	receive
		{ok, Path, Resource} -> ok;
		{no, Reason} -> {no, Reason}
	after
			1000 -> {no, timeout}
	end.

name() -> Siteid.
%% 
%% Local Functions
%%	
