%% Author: jens
%% Created: 10 feb 2013
%% Description: TODO: Add description to hty_shift_resource
-module(hty_shift_resource).
-record(hty_shift_resource, {var, subs}).
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
new(Var, Subs) ->
    #hty_shift_resource{var=Var, subs=Subs}.

handle(Htx, This) ->
    Var = This#hty_shift_resource.var,
    Subs = This#hty_shift_resource.subs,
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

