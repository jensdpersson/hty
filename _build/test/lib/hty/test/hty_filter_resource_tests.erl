%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_filter_resource_tests
-module(hty_filter_resource_tests).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([empty_chain_test/0, noop_filter_test/0]).

%%
%% API Functions
%%
empty_chain_test() ->
	Dut = hty_filter_resource:new({hty_noop_resource}, []),
	Htx = mockhtx,
	Htx = hty_filter_resource:handle(Htx, Dut).

noop_filter_test() ->
	Res = hty_filter_resource:new({hty_noop_resource}, [hty_noop_filter]),
	Htx = mockhtx,
	Htx = hty_resource:invoke_handle(Htx, Res).


%%
%% Local Functions
%%
