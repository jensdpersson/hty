-module(hty_site_lookup).

%%
%% Include files
%%
%%
%% Exported Function
%%
-export([mount/3, lookup/2, name/1, new/2]).

-record(hty_site_lookup, {siteid,sitepid}).
%%
%% API Functions
%%

new(Siteid, Sitepid) -> 
    #hty_site_lookup{siteid=Siteid, sitepid=Sitepid}.

lookup(Path, This) ->
    {_mod, _Siteid, Sitepid} = This,
    Sitepid ! {lookup, Path, self()},
    receive
	{ok, Res} -> {ok, Res};
	{no, Path} -> {no, novalue}
    after 
	1000 -> {no, timeout}
    end.
		
mount(Path, Resource, This) ->
    {_Mod, _Siteid, Sitepid} = This,
    Sitepid ! {mount, Path, Resource, self()},
    receive
	{ok, Path, Resource} -> ok;
	{no, Reason} -> {no, Reason}
    after
	1000 -> {no, timeout}
    end.

name(This) -> 
    This#hty_site_lookup.siteid.
%% 
%% Local Functions
%%	
