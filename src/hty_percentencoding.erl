%% Author: jens
%% Created: 29 mar 2013
%% Description: TODO: Add description to hty_percentencoding
-module(hty_percentencoding).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([encode/1, decode/1, decode_each/1]).

%%
%% API Functions
%%
encode(Str) when is_list(Str) ->
	encode(list_to_binary(Str));
encode(Bin) ->
		encode(Bin, <<>>).
decode(Bin) ->
		decode(Bin, <<>>).

-spec decode_each(Input::list(list())) -> list(list()).
decode_each(Input) ->
	lists:map(fun(Item) ->
		binary_to_list(decode(Item))
	end, Input).

%%
%% Local Functions
%%
encode(In, Out) ->
		{A, B} = hty_scan:until(In, 32),
		case B of
			<<>> ->
				<<Out/binary, A/binary>>;
			<<32, Rest/binary>> ->
				encode(Rest, <<Out/binary, A/binary, "%20">>)
		end.

decode(In, Out) when is_list(In) ->
	decode(list_to_binary(In), Out);
decode(In, Out) ->
	{A, B} = hty_scan:until_either(In, $%, $+),
	case B of
		<<>> ->
			<<Out/binary, A/binary>>;
    <<$+, Rest/binary>> ->
      Int = 32,
		  Out1 = <<Out/binary, A/binary, Int:8/integer>>,
		  decode(Rest, Out1);
		<<$%, H1:8/integer, H2:8/integer, Rest/binary>> ->
			Int = dehex(H1) * 16 + dehex(H2),
			Out1 = <<Out/binary, A/binary, Int:8/integer>>,
		  decode(Rest, Out1)
	end.

dehex($0) -> 0;
dehex($1) -> 1;
dehex($2) -> 2;
dehex($3) -> 3;
dehex($4) -> 4;
dehex($5) -> 5;
dehex($6) -> 6;
dehex($7) -> 7;
dehex($8) -> 8;
dehex($9) -> 9;
dehex($a) -> 10;
dehex($b) -> 11;
dehex($c) -> 12;
dehex($d) -> 13;
dehex($e) -> 14;
dehex($f) -> 15;
dehex($A) -> 10;
dehex($B) -> 11;
dehex($C) -> 12;
dehex($D) -> 13;
dehex($E) -> 14;
dehex($F) -> 15.
