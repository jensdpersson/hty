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
-export([first_match/2]).

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
	


%%
%% Local Functions
%%

