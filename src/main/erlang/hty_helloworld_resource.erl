%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_public_resource
-module(hty_helloworld_resource).

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
	Htx1 = Htx:rsp_header('Content-Type', "text/html"),
	Htx1:out({binary, <<"<html><h1>Hello World!</h1></html>">>}).

%%
%% Local Functions
%%

