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
	Dut = hty_filter_resource:new(hty_echo_resource, []),
	Htx = mockhtx,
	Htx = Dut:handle(Htx).

noop_filter_test() ->
	Dut = hty_filter_resource:new(hty_echo_resource, [hty_noop_filter]),
	Htx = mockhtx,
	Htx = Dut:handle(Htx).


%%
%% Local Functions
%%
