%% Author: jens
%% Created: 14 feb 2013
%% Description: TODO: Add description to hty_uri
-module(hty_uri).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([parse_path/1]).

%%
%% API Functions
%%
-spec parse_path(binary()) -> {[Segments::binary()], [QueryParams::binary()]}.
parse_path(Path) when is_binary(Path) ->
	parse_path([], Path).
		
%%
%% Local Functions
%%
parse_path(Segs, Input) ->
	{Token, Rest} = hty_util:until_either(Input, $/, $?),
	Segs1 = case Token of
						<<>> ->
							Segs;
						_ ->
							[Token|Segs]
					end,
	case Rest of
		<<$/, Input1/binary>> ->
			parse_path(Segs1, Input1);
		<<$?, Input1/binary>> ->
			Segs2 = lists:reverse(Segs1),
			parse_query([], Input1, Segs2);
		<<>> ->
			Segs2 = lists:reverse(Segs1),
			{Segs2, []}
	end.

parse_query(Params, Input, Segs) ->
	{Token, Rest} = hty_util:until(Input, $&),
	KeyValue = case hty_util:until(Token, $=) of  
							 {Key, <<$=, Value/binary>>} ->
								 {Key, Value};
							 Kv ->
								 Kv
						 end,
	Params1 = [KeyValue|Params],
	case Rest of
		<<$&, Input1>> ->
			parse_query(Params1, Input1, Segs);
		<<>> ->
			{Segs, lists:reverse(Params1)}
	end.
