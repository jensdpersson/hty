%% Author: jens
%% Created: 16 feb 2013
%% Description: TODO: Add description to hty_slots
-module(hty_slots).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([parse/1, fill/2]).

%%
%% API Functions
%%
-type slots() :: list().

-spec parse(binary()) -> slots()|{no, any()}.
parse(Input) ->
	find_start(Input, []).

-spec fill(slots(), list()) -> iolist()|{no, any()}.
fill(Slots, Data) ->
	io:format("Merging ~p into ~p~n", [Data, Slots]),
	fill_next(Slots, Data, []).
	
%%
%% Local Functions
%%
find_start(Input, Output) ->
	{Token, Rest} = hty_scan:until(Input, ${),
	case Rest of
		<<${, Input1/binary>> ->
			find_end(Input1, [Token|Output]);
		<<>> ->	
			lists:reverse([Token|Output])
	end.

find_end(Input, Output) ->
	{Token, Rest} = hty_scan:until(Input, $}),
	case Rest of
		<<$}, Input1/binary>> ->
			find_start(Input1, [{key, Token}|Output]);
		<<>> ->	
			{no, "unmatched"}
	end.

fill_next([{key, Key}|Rest], Data, Output) -> 
	case lists:keyfind(Key, 1, Data) of
		false ->
			{no, {"missingkey", Key}};
		{Key, Value} ->
			fill_next(Rest, Data, [Value|Output])
	end;
fill_next([], _, Output) ->
	lists:reverse(Output);
fill_next([Binary|Rest], Data, Output) ->
	fill_next(Rest, Data, [Binary|Output]).

			



