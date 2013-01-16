%% Author: jens
%% Created: 5 jan 2013
%% Description: TODO: Add description to hty_formtree
-module(hty_formtree_spaf).

%%
%% Exported Functions
%%
-export([parse/1, parse/2]).

%%
%% API Functions
%%

parse(Data) ->
	Q0 = {{expect_eq, <<>>}, []},
	parse(Data, Q0).

parse(Data, {Expect, Stack}) ->
	case Data of
		eos ->
			case Expect of
				{expect_eq, _Key} ->
					{no, missing_eq};
				{expect_amp, Key, Value} ->
					case make_out(Key, Value, Stack) of
						{no, Reason} ->
							{no, Reason};
						{[], O1} ->
							{ok, done, [O1]};
						{_S, _O1} ->
							{no, {badeos, stacknotempty}}
					end
			end;	
		_ ->
			case p(Data, Expect, Stack, []) of
				{no, Reason} ->
					{no, Reason};
				{ok, State, Outs} ->
					{ok, State, lists:reverse(Outs)}
			end
	end.
	
%%
%% Local Functions
%%
p(I, E, S, O) ->
	case E of
		{expect_eq, KeySoFar} ->
			{MoreKey, Rest} = until(I, $=),
			Key = <<KeySoFar/binary, MoreKey/binary>>,
			case Rest of
				<<"">> ->
					{ok, {{expect_eq, Key}, S}, O};
				<<$=, I2/binary>> ->
					p(I2, {expect_amp, Key, <<>>}, S, O)
			end;
		{expect_amp, Key, ValueSoFar} ->
			{MoreValue, Rest} = until(I, $&),
			Value = <<ValueSoFar/binary, MoreValue/binary>>,
			case Rest of
				<<"">> ->
					{ok, {{expect_amp, Key, Value}, S}, O};
				<<$&, I2/binary>> ->
					case make_out(Key, Value, S) of
						{no, Reason} ->
							{no, Reason};
						{S1, O1} ->
							p(I2, {expect_eq, <<>>}, S1, [O1|O])
					end
			end
	end.

-spec make_out(binary(), binary(), list()) -> 
				{no, any()} | {list(), any()}.
make_out(Key, Value, Stack) ->
	case Key of
		<<$[>> ->
			{[Value|Stack], {push, Value}};
		<<$">> ->
			{Stack, {text, Value}};
		<<$]>> ->
			case Stack of
				[Value|Qs] -> 
					{Qs, {pop, Value}};
				[Other|_] ->
					{no, {badpop, Value, Other}};
				[] ->
					{no, {badpop, Value, empty}}
			end;
		_ ->
			{Stack, {kv, Key, Value}}
	end.

until(Data, Char) ->
	until(0, Data, Char).

until(Index, B, Char) ->
	case B of
		<<Prefix:Index/binary, Char:8/integer, Rest/binary>> ->
			{Prefix, <<Char/integer,Rest/binary>>};
		<<Prefix:Index/binary, "">> ->
			{Prefix, <<"">>};
		Other when is_binary(Other) ->
			until(Index+1, B, Char)
	end.


				
