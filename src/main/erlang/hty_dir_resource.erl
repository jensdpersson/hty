%% Author: jens
%% Created: 9 feb 2013
%% Description: TODO: Add description to hty_dir_resource
-module(hty_dir_resource, [Segment, Subs]).

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
	case Htx:consume(Segment) of
		no ->
			Htx:not_found();
		Htx1 ->
			Htx1:dispatch(Subs)
	end.


%%
%% Local Functions
%%

