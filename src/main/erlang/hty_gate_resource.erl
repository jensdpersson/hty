%% Author: jens
%% Created: 7 feb 2013
%% Description: TODO: Add description to hty_gate_resource
-module(hty_gate_resource, [Lookup, Subs]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/1]).

%%
%% API Functions
%%
handle(Htx) ->
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

