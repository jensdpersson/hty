%% Author: jens
%% Created: 23 mar 2013
%% Description: TODO: Add description to hty_listing_resource
-module(hty_listing_resource).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/2]).
-import(hty_listing, [list/2]).
-record(hty_listing_resource, {filename, fspath}).
%%
%% API Functions
%%

new(Filename, Fspath) ->
    #hty_listing_resource{filename=Filename, fspath=Fspath}.

handle(Htx, This) ->
    Filename = filename(This),
    case Htx:path_below() of
	[] ->
	    list_on_get(Htx, This);
	[Filename] ->
	    list_on_get(Htx, This);
	_ ->
	    Htx:not_found()
    end.	

%%
%% Local Functions
%%
list_on_get(Htx, This) ->
    case Htx:method() of
	'GET' ->
	    list(Htx, This#hty_listing_resource.fspath);	
	_ ->
	    Htx:method_not_allowed(['GET'])
    end.

filename(This) ->
    This#hty_listing_resource.filename.
	

	
