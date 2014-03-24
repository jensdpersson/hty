%% Author: jens
%% Created: 17 apr 2013
%% Description: TODO: Add description to hty_flatstorage_rule
-module(hty_flatstorage_rule).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([match/2]).

%%
%% API Functions
%%
match(Fspath, _) ->
	%Fspath = Walker:fspath(),
	case lists:reverse(Fspath:parts()) of
		["flat"|_] ->
			Tofs = fun(Uripath) ->
									Fspath:subpath(Uripath)
						 end,
			{claim, {resource, hty_storage_resource:new(Tofs)}};
		_ ->
			next
	end.


%%
%% Local Functions
%%
