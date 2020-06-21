-module(hty_urilist).

-export([parse_binary/1, pack/1]).

parse_binary(Binary) ->
	parse_binary_internal(Binary, []).

parse_binary_internal(<<"">>, Acc) ->
	lists:reverse(Acc);
parse_binary_internal(Rest, Acc) ->
	{Line, Lines} = hty_scan:until_either(Rest, 13, 10),
	Rest2 = case Lines of
				<<13, 10, Rest1/binary>> ->
					Rest1;
				<<13, Rest1/binary>> ->
					Rest1;
				<<10, Rest1/binary>> ->
					Rest1;
				<<"">> ->
					<<"">>
			end,
	Acc1 = case Line of
			   <<"">> ->
				   Acc;
			   _ ->
				   [Line|Acc]
		   end,
	parse_binary_internal(Rest2, Acc1).
	
pack(Stringlist) ->
    lists:flatmap(fun(String) ->
        [String, "\r\n"]
    end, Stringlist).