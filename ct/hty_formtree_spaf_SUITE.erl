%% Author: jens
%% Created: 7 jan 2013
%% Description: TODO: Add description to hty_formtree_resource_tests
-module(hty_formtree_spaf_SUITE).
-compile(export_all).
%%
%% Include files
%%

-import(hty_formtree_spaf, [parse/1, parse/2]).

%%
%% Exported Functions
%%
all() -> [
		single_attr,
		dual_attr,
		nesting,
		no_eq,
		split_attr,
		apor
].

%%
%% API Functions
%%
single_attr(_C) ->
	{ok, Q1, []} = parse(<<"apa=lim">>),
	{ok, _, [{kv, <<"apa">>, <<"lim">>}]} = parse(eos, Q1).

dual_attr(_C) ->
	{ok, Q1, [{kv, <<"apa">>,<<"lim">>}]} = parse(<<"apa=lim&rim=bob">>),
	{ok, _, [{kv, <<"rim">>,<<"bob">>}]} = parse(eos, Q1).

nesting(_C) ->
	{ok, Q1, []} = parse(<<"[=contactbook">>),
	{ok, Q2, [{push, <<"contactbook">>}]} = parse(<<"&]=contactbook">>, Q1),
	{ok, _, [{pop, <<"contactbook">>}]} = parse(eos, Q2).

no_eq(_C) ->
	{ok, Q1, []} = parse(<<"lim">>),
	{no, missing_eq} = parse(eos, Q1).

split_attr(_C) ->
	{ok, Q1, []} = parse(<<"lim=ko">>),
	{ok, Q2, []} = parse(<<"rv">>, Q1),
	{ok, _Q3, [{kv, <<"lim">>, <<"korv">>}]} = parse(eos, Q2).

apor(_C) ->
	Input = <<"[=apor&[=apa&\"=orangutang&]=apa&]=apor">>,
	Facit = [
		{push, <<"apor">>},
		{push, <<"apa">>},
		{text, <<"orangutang">>},
		{pop, <<"apa">>},
		{pop, <<"apor">>}
	],
	{ok, Q1, Out} = parse(Input),
	{ok, Q2, Out2} = parse(eos, Q1),
	Facit = Out ++ Out2.
%%
%% Local Functions
%%
