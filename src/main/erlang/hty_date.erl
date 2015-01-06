-module(hty_date).

-export([parse/1]).
-export([year/1, month/1, day/1]).

-record(hty_date, {year=0, month=1, day=1}).

parse(Iso) when is_binary(Iso) ->
    parse(binary_to_list(Iso));
parse(Iso) ->
    [Y, M, D] = strings:split(Iso),
    #hty_date{year=Y, month=M, day=D}.

year(This) ->
    This#hty_date.year.

month(This) ->
    This#hty_date.month.

day(This) ->
    This#hty_date.day.
