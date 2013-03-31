%% Author: jens
%% Created: 29 mar 2013
%% Description: TODO: Add description to hty_percentencoding_tests
-module(hty_percentencoding_tests).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([decode_test/0, decode2_test/0]).

%%
%% API Functions
%%
decode_test() ->
	Input = <<"%5Blim/%5D">>,
	Facit = <<$[, "lim/", $]>>,
	Facit = hty_percentencoding:decode(Input).

decode2_test() ->
	Input = <<"LIM%5Br%e4ksm%f6rg%e5s/%5DBOLLHAV">>,
	Facit = <<"LIM[räksmörgås/]BOLLHAV">>,
	Facit = hty_percentencoding:decode(Input).


%%
%% Local Functions
%%

