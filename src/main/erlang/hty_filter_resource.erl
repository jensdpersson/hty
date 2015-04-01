%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_filter_resource
-module(hty_filter_resource).
-record(hty_filter_resource, {resource, filters}).

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
new(Resource, Filters) ->
    #hty_filter_resource{resource=Resource,filters=Filters}.

handle(Htx, This) ->
    F = fun(Module, Next) ->
		    Module:new(Next)
	  end,
    Chain = lists:foldl(F,
                  This#hty_filter_resource.resource,
                  This#hty_filter_resource.filters),
    Chain:handle(Htx).

%%
%% Local Functions
%%
