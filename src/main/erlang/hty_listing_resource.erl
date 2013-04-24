%% Author: jens
%% Created: 23 mar 2013
%% Description: TODO: Add description to hty_listing_resource
-module(hty_listing_resource, [Filename, Fspath]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/1]).
-import(hty_listing, [list/2]).

%%
%% API Functions
%%
handle(Htx) ->
	case Htx:path_below() of
		[] ->
			list_on_get(Htx);
		[Filename] ->
			list_on_get(Htx);
		_ ->
			io:format("Declining INDEX on ~p~n", [Htx:path_below()]),
			Htx:not_found()
	end.	

%%
%% Local Functions
%%
list_on_get(Htx) ->
	case Htx:method() of
		'GET' ->
			list(Htx, Fspath);	
		_ ->
			Htx:method_not_allowed(['GET'])
	end.

	

	
