%% Author: jens
%% Created: 10 feb 2013
%% Description: TODO: Add description to hty_shift_resource
-module(hty_shift_resource, [Var, Subs]).

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
	case Htx:consume() of
		no ->
			Htx:not_found();
		{Val, Htx1} ->
			Htx2 = Htx1:bind(Var, Val),
			Htx2:dispatch(Subs)
	end.


%%
%% Local Functions
%%

