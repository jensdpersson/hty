-module(hty_date).

-export([parse/1, format/1, with/3]).
-export([year/1, month/1, day/1]).

-record(hty_date, {year=0, month=1, day=1, hour=0, minute=0, second=0, milli=0}).

format(This) ->
  pad(This#hty_date.year, 999) ++ "-" ++
  pad(This#hty_date.month, 9) ++ "-" ++
  pad(This#hty_date.day, 9) ++ "T" ++
  pad(This#hty_date.hour, 9) ++ ":" ++
  pad(This#hty_date.minute, 9) ++ ":" ++
  pad(This#hty_date.second, 9) ++ "." ++
  pad(This#hty_date.milli, 99).

pad(Int, Lim) ->
  case Int > Lim of
    true -> integer_to_list(Int);
    false -> "0" ++ integer_to_list(Int)
  end.

parse(Iso) when is_binary(Iso) ->
    parse(binary_to_list(Iso));
parse(Iso) ->
  Slots = [
    {year,4,"-"},
    {month,2,"-"},
    {day,2,"T"},
    {hour,2,":"},
    {minute,2,":"},
    {second,2,"."},
    {milli,3,none}
  ],
  Ret = lists:foldl(fun(Slot, AccIn) ->
    case AccIn of
      {error, Error} ->
        {error, Error};
      {Date, Input} ->
        parse_slot(Slot, Input, Date)
    end
  end, {#hty_date{}, Iso}, Slots),
  case Ret of
    {error, Error} ->
      {error, Error};
    {Date, _} ->
      Date
  end.

parse_slot({Slot, Len, Delim}, In, Date) ->
  {Sublist, Rest} = lists:split(Len, In),
  case string:to_integer(Sublist) of
    {Int, []} ->
      case Rest of
        [Delim|Rest2] ->
          {ok, Date:with(Slot, Int), Rest2};
        [Other|_] ->
          {error, "Unexpected token " ++ Other}
      end;
    {_, [Other|_]} ->
      {error, "Unexpected token " ++ Other}
  end.

with(year, V, This) ->
  This#hty_date{year=V};
with(month, V, This) ->
  This#hty_date{month=V};
with(day, V, This) ->
  This#hty_date{day=V};
with(hour, V, This) ->
  This#hty_date{hour=V};
with(minute, V, This) ->
  This#hty_date{minute=V};
with(second, V, This) ->
  This#hty_date{second=V};
with(milli, V, This) ->
  This#hty_date{milli=V}.



year(This) ->
    This#hty_date.year.

month(This) ->
    This#hty_date.month.

day(This) ->
    This#hty_date.day.
