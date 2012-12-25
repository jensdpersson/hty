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
%% -export([create/2]).
%-export([mount/3, lookup/2]).

%%
%% API Functions
%%
%% Start a new process for the site id given. Return and store a reference.
%% start(SiteId) ->
%%     Pid = spawn(fun() -> loop([]) end),
%%     register(key(SiteId), Pid),
%%     ok.
%% 
%% lookup(SiteId, Path) ->
%%     key(SiteId) ! {lookup, Path, self()},
%%     receive
%% 	{ok, Res} -> {ok, Res};
%% 	{no, Path} -> {no, novalue}
%%     after 
%% 	1000 -> {no, timeout}
%%     end.
		
%mount(SiteId, Path, Resource) ->
%    key(SiteId) ! {mount, Path, Resource, self()},
%    receive
%	{ok, Path, Resource} -> ok;
%	{no, Reason} -> {no, Reason}
%    after
%	1000 -> {no, timeout}
%    end.

%stop(SiteId) ->
%    key(SiteId) ! {stop, self()},
%    receive
%	{ok, stopping} -> ok;
%	{no, Error} -> {no, Error}
%    after 
%	1000 -> {no, timeout}
 %   end.

%% 
%% Local Functions
%%	

%%
%% Local Functions
%%
%loop_registry(Sites) ->
%    receive 
%	{add, Id, Pid, ReplyTo} ->
%	    case lists:keyfind(Id, 1, Sites) of
%		false -> 
%		    ReplyTo ! {ok, added},
%		    loop_registry([{Id, Pid}|Sites]);
%		{Id, Pid} ->
%		    ReplyTo ! {ok, already_exists},
%		    loop_registry(Sites);
%		{Id, Pid2} ->
%		    ReplyTo ! {no, conflict},
%		    loop_registry(Sites)
%	    end;
%	{drop, Id, ReplyTo} ->
%	    case lists:keytake(Id, 1, Sites) of
%		false -> 
%		    ReplyTo ! {ok, already_gone};
%		{value, _, Sites1} ->
%		    ReplyTo ! {ok, removing},
%		    loop_registry(Sites1)
%	    end;
%	{list, ReplyTo} ->
%	    ReplyTo ! Sites,
%	    loop_registry(Sites);
%	{stop, ReplyTo} ->
%	    ReplyTo ! ok
%   end.

%loop(State) ->
%    receive
%	{list, Path, ReplyTo} ->
%	    ReplyTo ! not_implemented,
%	    loop(State);
%	{mount, Path, Resource, ReplyTo} ->
%	    io:format("bollhav~n"),
%	    State1 = trema:insert(State, Path, Resource),
%	    io:format("New state ~p~n", [State1]),
%	    ReplyTo ! {ok, Path, Resource},
%	    loop(State1);
%	{lookup, Path, ReplyTo} ->
%	    ReplyTo ! case trema:lookup(State, Path) of
%			  [] -> {no, Path};
%			  Res -> {ok, Res}
%		      end,
%	    loop(State);
%	{stop, ReplyTo} ->
%	    ReplyTo ! stopping,
%	    ok
 %   end.

%key(Siteid) -> list_to_atom("hty_site_" ++ atom_to_list(Siteid)).
