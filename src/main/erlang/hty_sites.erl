%% Author: jens
%% Created: 13 aug 2012
%% Description: TODO: Add description to hty_sites
-module(hty_sites).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([create/1]).

%%
%% API Functions
%%
create(Siteid) ->
	Pid = spawn(fun() -> loop([]) end),
	hty_site:new(Siteid, Pid).

%%
%% Local Functions
%%
loop(State) ->
    receive
	{list, Path, ReplyTo} ->
	    ReplyTo ! not_implemented,
		loop(State);
	{mount, Path, Resource, ReplyTo} ->
		io:format("bollhav~n"),
		State1 = trema:insert(State, Path, Resource),
		io:format("New state ~p~n", [State1]),
		ReplyTo ! {ok, Path, Resource},
		loop(State1);
	{lookup, Path, ReplyTo} ->
	    ReplyTo ! case trema:lookup(State, Path) of
			[] -> {no, Path};
			Res -> {ok, Res}
		end,
		loop(State);
	{stop} -> 
	    ok
    end.
