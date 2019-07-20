%% Author: jens
%% Created: 11 mar 2013
%% Description: TODO: Add description to hty_index_resource
-module(hty_index_resource).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/2]).
-record(hty_index_resource, {fspath, sub}).
%%
%% API Functions
%%

new(Fspath, Sub) ->
    #hty_index_resource{fspath=Fspath, sub=Sub}.

handle(Htx, This) ->
    Fspath = This#hty_index_resource.fspath,
    Sub = This#hty_index_resource.sub,
    Htx1 = case Htx:path_below() of
	       [] ->
		   Htx:path_below([Fspath:basename()]);
	       _ -> 
		   Htx 
	   end,
    Sub:handle(Htx1).

%%
%% Local Functions
%%

