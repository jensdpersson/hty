%% Author: jens
%% Created: 7 jan 2013
%% Description: TODO: Add description to hty_formtree_resource_tests
-module(hty_formtree_spaf_tests).

%%
%% Include files
%%

-import(hty_formtree_spaf, [parse/1, parse/2]).

%%
%% Exported Functions
%%
-export([
				 single_attr_test/0,
				 dual_attr_test/0,
				 nesting_test/0,
				 no_eq_test/0,
				 split_attr_test/0
				 ]).

%%
%% API Functions
%%
single_attr_test() ->
	{ok, Q1, []} = parse(<<"apa=lim">>),
	{ok, _, [{kv, <<"apa">>, <<"lim">>}]} = parse(eos, Q1).

dual_attr_test() ->
	{ok, Q1, [{kv, <<"apa">>,<<"lim">>}]} = parse(<<"apa=lim&rim=bob">>),
	{ok, _, [{kv, <<"rim">>,<<"bob">>}]} = parse(eos, Q1).

nesting_test() ->
	{ok, Q1, []} = parse(<<"[=contactbook">>),
	{ok, Q2, [{push, <<"contactbook">>}]} = parse(<<"&]=contactbook">>, Q1),
	{ok, _, [{pop, <<"contactbook">>}]} = parse(eos, Q2).

no_eq_test() ->
	{ok, Q1, []} = parse(<<"lim">>),
	{no, missing_eq} = parse(eos, Q1).
	
split_attr_test() ->
	{ok, Q1, []} = parse(<<"lim=ko">>),
	{ok, Q2, []} = parse(<<"rv">>, Q1),
	{ok, _Q3, [{kv, <<"lim">>, <<"korv">>}]} = parse(eos, Q2).
	
%%
%% Local Functions 
%%

