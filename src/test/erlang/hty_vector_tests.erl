%% Author: jens
%% Created: 13 aug 2012
%% Description: TODO: Add description to hty_vector_tests
-module(hty_vector_tests).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([basic_test/0]).

%%
%% API Functions
%%

basic_test() -> 
	Dut = setup(),
	Old = [{wrap, a}, {wrap, b}, {wrap, c}, {wrap, d}],
	New = [b, e, d],
	Facit = [{wrap, e}, {wrap, d}, {wrap, b}],
	io:format("Checking filtered data"),
	Facit = Dut:filter(Old, New),
	io:format("Checking culled elements"),
	[{wrap, c}, {wrap, a}] = get(culled).
	
%%
%% Local Functions
%%
setup() ->
	put(culled, []),
	hty_vector:new(fun cmp/2, fun cull/1, fun ctor/1).
	
cmp({wrap,A}, A) -> true;
cmp(_A, _B) -> false.

cull(M) ->
	Culled = get(culled),
	put(culled, [M|Culled]).		

ctor(M) -> {wrap, M}.
