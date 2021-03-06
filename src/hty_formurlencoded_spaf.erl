%% Author: jens
%% Created: 5 jan 2013
-module(hty_formurlencoded_spaf).

%%
%% Exported Functions
%%
-export([parse/2]).
-import(hty_scan, [until/2]).
-import(hty_percentencoding, [decode/1]).
%%
%% API Functions
%%

parse(Data, q0) ->
	parse(Data, {{expect_eq, <<>>}, []});

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
							{ok, done, [O1, eos]};
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
				<<>> ->
					{ok, {{expect_eq, Key}, S}, O};
				<<$=, I2/binary>> ->
					p(I2, {expect_amp, Key, <<>>}, S, O)
			end;
		{expect_amp, Key, ValueSoFar} ->
			{MoreValue, Rest} = until(I, $&),
			Value = <<ValueSoFar/binary, MoreValue/binary>>,
			case Rest of
				<<>> ->
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
	{Stack, {kv, decode(Key), decode(Value)}}.
