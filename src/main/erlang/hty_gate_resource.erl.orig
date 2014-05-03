%% Author: jens
%% Created: 7 feb 2013
%% Description: TODO: Add description to hty_gate_resource
-module(hty_gate_resource, [Role, Subs]).

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
	{_Nick, Roles} = Htx:loggedin(),
	case lists:member(Role, Roles) of
		true ->
			Htx:dispatch(Subs);
		false ->
			Htx:forbidden()
	end.

%%
%% Local Functions
%%

