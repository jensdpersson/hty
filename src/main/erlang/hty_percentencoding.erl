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
-export([encode/1, decode/1]).

%%
%% API Functions
%%
encode(_Bin) -> not_implemented.
decode(Bin) -> 
		decode(Bin, <<>>).

%%
%% Local Functions
%%

decode(In, Out) ->
	{A, B} = hty_util:until(In, $%),
	case B of 
		<<>> ->
			<<Out/binary, A/binary>>;
		<<$%, H1:8/integer, H2:8/integer, Rest/binary>> ->
			Int = dehex(H1) * 16 + dehex(H2),
			Out1 = <<Out/binary, A/binary, Int:8/integer>>,	
		  decode(Rest, Out1)
	end.

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
