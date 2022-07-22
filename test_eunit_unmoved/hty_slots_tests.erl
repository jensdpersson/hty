%% Author: jens
%% Created: 16 feb 2013
%% Description: TODO: Add description to hty_slots_test
-module(hty_slots_tests).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
	middle_test/0,
	start_test/0, 
	end_test/0, 
	bad_test/0,
	missing_test/0,
	many_test/0
	]).

%%
%% API Functions
%%
middle_test() ->
	Input = <<"Hello {locus}!">>,
	Data = [{<<"locus">>, <<"World">>}],
	test_it(Input, Data, [<<"Hello ">>, <<"World">>, <<"!">>]).

start_test() ->
	Input = <<"{greeting} World!">>,
	Data = [{<<"greeting">>, <<"Hello">>}],
	test_it(Input, Data, [<<>>, <<"Hello">>, <<" World!">>]).

end_test() ->
	Input = <<"Hello World{punctuation}">>,
	Data = [{<<"punctuation">>, <<"!">>}],
	test_it(Input, Data, [<<"Hello World">>, <<"!">>, <<>>]).

bad_test() ->
		{no, "unmatched"} = hty_slots:parse(<<"Hello{World!">>).

missing_test() ->
	Slots = hty_slots:parse(<<"{greeting} {locus}!">>),
	{no, {"missingkey", <<"locus">>}} =
		hty_slots:fill(Slots, [{<<"greeting">>, <<"Hello">>}]).

many_test() ->
	Slots = hty_slots:parse(<<"{greeting} {locus}!">>),
	Data = [{<<"greeting">>, <<"Hello">>}, 
					{<<"locus">>, <<"World">>}],
	Facit = [<<>>,<<"Hello">>,<<" ">>,<<"World">>, <<"!">>],
	Facit = hty_slots:fill(Slots, Data).


%%
%% Local Functions
%%
test_it(Input, Data, Facit) ->
	Slots = hty_slots:parse(Input),
	io:format("Slots=[~p]~n", [Slots]),
	Facit = hty_slots:fill(Slots, Data).
	