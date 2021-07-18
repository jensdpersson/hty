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
-export([fold/3,
         find/2,
         keyappend/3]).
-export([ltrim/1, rewind/2, fast_forward/2]).

-export([std_rule_match/3]).
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

-spec fold(any(), any(), any()) -> {nobreak, any()} | {break, any(), any()}.
fold(_, Acc, []) -> {nobreak, Acc};
fold(Fun, Acc, [X|Xs]) ->
	case Fun(X, Acc) of
		{break, Data} -> {break, Data, [X|Xs]};
		{next, Data} -> fold(Fun, Data, Xs)
	end.

rewind([], Bs) -> Bs;
rewind([A|As], Bs) ->
    rewind(As, [A|Bs]).

fast_forward([], Bs) ->
	{ok, Bs};
fast_forward([A|As], [A|Bs]) ->
	fast_forward(As, Bs);
fast_forward(_, Bs) ->
	{ok, Bs}.

% Find the record in Entries whose 1st element is Key.
% Append Item to the 2nd element.
% If there is no such record, append one.
keyappend(Key, Item, Entries) ->
	keyappend_internal(Key, Item, [], Entries).
	
% Final case. No matching record existed, so add one.	
keyappend_internal(Key, Item, NotOneOfThese, []) ->
    [{Key, [Item]}|NotOneOfThese];
% Final case. A matching record found. Append item and return.  
keyappend_internal(Key, Item, NotOneOfThese, [{Key, Items}|Entries]) ->
    rewind(NotOneOfThese, [{Key, [Item|Items]}|Entries]);
% Recursive case.    
keyappend_internal(Key, Item, NotOneOfThese, [Entry|Entries]) ->
    keyappend_internal(Key, Item, [Entry|NotOneOfThese], Entries).

%to_binary(MixedList) ->
%	to_binary_internal(MixedList, <<"">>).
%to_binary_internal([], Acc) ->
%	Acc;
%to_binary_internal([A|As], Acc) when is_binary(A) ->
%	to_binary_internal(As, <<A/binary, Acc/binary>>);
%to_binary_internal([A|As], Acc) when is_list(A) ->
%	to_binary_internal(As, to_binary_internal(A, Acc));
%to_binary_internal([A|As], Acc) when is_binary(A) ->
%	to_binary_internal(As, <<A/binary, Acc/binary>>);

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

std_rule_match(Ext, Res, Walker) ->
	Fs = Walker:fspath(),
	case lists:reverse(Fs:parts()) of
		[Ext, Param | _] ->
			Subs = Walker:subs(),
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
