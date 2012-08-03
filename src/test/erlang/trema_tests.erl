%% Author: jens
%% Created: 25 jul 2012
%% Description: TODO: Add description to trema_tests
-module(trema_tests).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([insert_test/0]).

%%
%% API Functions
%%

insert_test() ->
	T = trema:new(),
	[{k,v,[]}] = trema:insert(T, k, v).


%%
%% Local Functions
%%

