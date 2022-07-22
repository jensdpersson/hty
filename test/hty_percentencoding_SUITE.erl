%% Author: jens
%% Created: 29 mar 2013
-module(hty_percentencoding_SUITE).

-export([all/0, decode_square_brackets/1, decode_latin1/1]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
all() -> [
	decode_square_brackets,
	decode_latin1
].

decode_square_brackets(_Cfg) ->
	Input = <<"%5Blim/%5D">>,
	Facit = <<$[, "lim/", $]>>,
	Facit = hty_percentencoding:decode(Input).

decode_latin1(_Cfg) ->
	Input = <<"LIM%5Br%e4ksm%f6rg%e5s/%5DBOLLHAV">>,
	Facit = <<"LIM[räksmörgås/]BOLLHAV">>,
	Facit = hty_percentencoding:decode(Input).
