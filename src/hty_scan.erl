-module(hty_scan).

%%
%% Exported Functions
%%
-export([until/2, until/3]).
-export([until_either/3, until_either/4]).
-export([until_oneof/4, until_oneof/5]).

%%
%% API Functions
%%
-spec until(Data::binary(), Char::integer()) -> {binary(), binary()}.
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

until_oneof(Data, Either, Or, Else) ->
	until_oneof(0, Data, Either, Or, Else).

until_oneof(Index, B, C1, C2, C3) ->
	case B of
		<<Prefix:Index/binary, C1:8/integer, Rest/binary>> ->
			{Prefix, <<C1/integer,Rest/binary>>};
		<<Prefix:Index/binary, C2:8/integer, Rest/binary>> ->
			{Prefix, <<C2/integer,Rest/binary>>};
                <<Prefix:Index/binary, C3:8/integer, Rest/binary>> ->
			{Prefix, <<C3/integer,Rest/binary>>};
		<<Prefix:Index/binary, "">> ->
			{Prefix, <<"">>};
		Other when is_binary(Other) ->
			until_oneof(Index+1, B, C1, C2, C3)
	end.
