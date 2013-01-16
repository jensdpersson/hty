%% Author: jens
%% Created: 31 dec 2012
%% Description: TODO: Add description to hty_util
-module(hty_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([first_match/2, fold/3]).

%%
%% API Functions
%%
first_match(_Pred, []) ->
	no;
first_match(Pred, [X|Xs]) ->
	case Pred(X) of
		{ok, X1} -> {ok, X1};
		no -> first_match(Pred, Xs)
	end.

fold(_, Acc, []) -> {nobreak, Acc};
fold(Fun, Acc, [X|Xs]) ->
	case Fun(X, Acc) of
		{break, Data} -> {break, Data, [X|Xs]};
		{next, Data} -> fold(Fun, Data, Xs)
	end.

%%
%% Local Functions
%%

