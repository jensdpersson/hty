%% Author: jens
%% Created: 11 mar 2013
%% Description: TODO: Add description to hty_index_resource
-module(hty_index_resource, [Fspath, Sub]).

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
	Htx1 = case Htx:path_below() of
					 [] ->
						 Htx:path_below([Fspath:basename()]);
					 _ -> Htx 
				 end,
	Sub:handle(Htx1).

%%
%% Local Functions
%%

