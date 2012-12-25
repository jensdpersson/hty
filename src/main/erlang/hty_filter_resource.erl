%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_filter_resource
-module(hty_filter_resource, [Filters, Resource]).

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
	F = fun(Module, Next) -> 
				Module:new(Next)
		end, 
	Chain = lists:foldl(F, Resource, Filters),
	Chain:handle(Htx).

%%
%% Local Functions
%%


