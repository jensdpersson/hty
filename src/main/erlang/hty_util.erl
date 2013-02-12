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
-export([fold/3,find/2]).
-export([until/2, until/3]).
-export([until_either/3, until_either/4]).
-export([ltrim/1]).

-export([std_rule_match/4,subs/2]).

%%
%% API Functions
%%
find(_Pred, []) ->
	 no;
find(Pred, [X|Xs]) ->
	 case Pred(X) of
		 {ok, X1} -> {ok, X1};
		 no -> find(Pred, Xs)
	 end.

fold(_, Acc, []) -> {nobreak, Acc};
fold(Fun, Acc, [X|Xs]) ->
	case Fun(X, Acc) of
		{break, Data} -> {break, Data, [X|Xs]};
		{next, Data} -> fold(Fun, Data, Xs)
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

until_either(Data, Either, Or) ->
	until_either(0, Data, Either, Or).

until_either(Index, B, C1, C2) ->
	case B of
		<<Prefix:Index/binary, C1:8/integer, Rest/binary>> ->
			{Prefix, <<C1/integer,Rest/binary>>};
		<<Prefix:Index/binary, C2:8/integer, Rest/binary>> ->
			{Prefix, <<C2/integer,Rest/binary>>};
		<<Prefix:Index/binary, "">> ->
			{Prefix, <<"">>};
		Other when is_binary(Other) ->
			until_either(Index+1, B, C1, C2)
	end.

ltrim(Binary) when is_binary(Binary) ->
	case Binary of
		<<$\s, Rest/binary>> ->
			ltrim(Rest);
		<<$\t, Rest/binary>> ->
			ltrim(Rest);
		<<$\n, Rest/binary>> ->
			ltrim(Rest);
		<<$\r, Rest/binary>> ->
			ltrim(Rest);
		Other -> 
			Other
	end.

std_rule_match(Ext, Res, Fs, Rs) ->
	case lists:reverse(Fs:parts()) of
		[Ext, Param | _] ->
			Subs = subs(Fs, Rs),
			{claim, {resource, Res:new(Param, Subs)}};
		_ ->
			next
	end.

subs(Fspath, Rules) ->
	lists:flatmap(fun({ok, {resource, R}, _, _}) -> [R];
														 (_) -> []
													 end, Fspath:walk(Rules)).

%%
%% Local Functions
%%

