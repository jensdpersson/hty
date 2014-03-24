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
-export([ltrim/1, rewind/2]).

-export([std_rule_match/4]).
-export([parse_key_value_list/1]).

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

rewind([], Bs) -> Bs;
rewind([A|As], Bs) ->
    rewind(As, [A|Bs]).

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
			Subs = Fs:subs(Rs),
			{claim, {resource, Res:new(Param, Subs)}};
		_ ->
			next
	end.

parse_key_value_list(KeyValueList) ->
	lists:map(fun(KeyValue) ->
					  [Key, Value] = string:tokens(KeyValue, "="),
					  {Key, Value}
			  end, string:tokens(KeyValueList, ",")).

%%
%% Local Functions
%%

