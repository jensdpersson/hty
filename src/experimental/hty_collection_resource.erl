%% Author: jens
%% Created: 8 mar 2013
%% Description: TODO: Add description to hty_collection_resource
-module(hty_collection_resource, [Param, Subs]).

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
	case Htx:path_below() of
		[] ->
			% leaf case
			Htx:not_found();
		[Segment] ->
		  % twig case
			do_twig(Htx, Segment);
		_ ->
			% branch case
			Htx:dispatch(Subs)
	end.


%%
%% Local Functions
%%
do_twig(Htx, _Segment) ->
	case Htx:method() of
		'GET' ->
			_Storage = Htx:storage();
		'POST' ->
			Htx:method_not_allowed(['GET', 'PUT', 'DELETE']);
		 'PUT' ->
			 notyet;
		'DELETE' ->
			notyet
		end.
