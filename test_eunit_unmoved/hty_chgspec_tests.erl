%% Author: jens
%% Created: 9 dec 2012
%% Description: TODO: Add description to hty_rules_rule_tests
-module(hty_chgspec_tests).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([add_one_test/0, remove_one_test/0, multi_change_test/0]).

%%
%% API Functions
%%

add_one_test() -> test("+lim_bob", ["lim_bob"], []).
remove_one_test() -> test("-lim_bob", [], ["lim_bob"]).
multi_change_test() -> 
	test("+lim,-pim,+bob", ["bob", "lim"], ["pim"]).	


%%
%% Local Functions
%%
test(Spec, Add, Remove) ->
	Facit = {ok, Add, Remove},
	Facit = hty_chgspec:parse(Spec).
	