%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_public_resource
-module(hty_public_resource, [Fspath]).

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
	Htx:header({'Content-Type', "text/html"}),
	Htx:body(<<"<html><h1>Hello World!</h1></html>">>),
	Htx:ok().


%%
%% Local Functions
%%

