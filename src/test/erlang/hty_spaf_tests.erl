%% Author: jens
%% Created: 18 mar 2013
%% Description: TODO: Add description to hty_spaf_tests
-module(hty_spaf_tests).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([binder_noschema_test/0]).

%%
%% API Functions
%%
binder_noschema_test() ->
	Schema = free,
	Dut = hty_spaf:binder(Schema),
	%lists:foldl(Dut, [kv...kv...kv..eos])
	%kolla slutvärdet så alla fält är med.
	fail_with = Dut.

%%
%% Local Functions
%%

