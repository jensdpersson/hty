%% Author: jens
%% Created: 7 feb 2013
%% Description: TODO: Add description to hty_gate_resource
-module(hty_gate_resource).
-record(hty_gate_resource, {lookup, subs}).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/2]).

%%
%% API Functions
%%
new(Lookup, Subs) ->
    #hty_gate_resource{lookup=Lookup, subs=Subs}.

handle(Htx, This) ->
    Lookup = This#hty_gate_resource.lookup,
    Subs = This#hty_gate_resource.subs,
    io:format("Principal = ~p~n", [Htx:principal()]),
    {_Nick, Roles} = Htx:principal(),
    Role = Lookup(Htx),
    io:format("member? ~p ~p~n", [Role, Roles]),
    case lists:member(Role, Roles) of
	true ->
	    Htx:dispatch(Subs);
	false ->
	    Htx:forbidden()
    end.

%%
%% Local Functions
%%

