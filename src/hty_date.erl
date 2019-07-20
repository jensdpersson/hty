-module(hty_date).

-export([parse/1, format/1, format_date/1, with/3]).
-export([year/1, month/1, day/1]).

-export([now/0, tomorrow/1, daybreak/1, nightfall/1]).

-record(hty_date, {year=0, month=1, day=1, hour=0, minute=0, second=0, milli=0}).

now() ->
  Now = os:timestamp(),
  {{Year,Month,Day},{Hour,Minute,Second}} =
    calendar:now_to_universal_time(Now),
  {_,_,Micro} = Now,
  Milli = Micro div 1000,
  #hty_date{
    year=Year,
    month=Month,
    day=Day,
    hour=Hour,
    minute=Minute,
    second=Second,
    milli=Milli
  }.

format(This) ->
  pad(This#hty_date.year, 999) ++ "-" ++
  pad(This#hty_date.month, 9) ++ "-" ++
  pad(This#hty_date.day, 9) ++ "T" ++
  pad(This#hty_date.hour, 9) ++ ":" ++
  pad(This#hty_date.minute, 9) ++ ":" ++
  pad(This#hty_date.second, 9) ++ "." ++
  pad(This#hty_date.milli, 99).

format_date(This) ->
  pad(This#hty_date.year, 999) ++ "-" ++
  pad(This#hty_date.month, 9) ++ "-" ++
  pad(This#hty_date.day, 9).

pad(Int, Lim) ->
  case Int > Lim of
    true -> integer_to_list(Int);
    false -> "0" ++ integer_to_list(Int)
  end.

parse(Iso) when is_binary(Iso) ->
    parse(binary_to_list(Iso));
parse(Iso) ->
  Slots = [
    {year,4,$-},
    {month,2,$-},
    {day,2,$T},
    {hour,2,$:},
    {minute,2,$:},
    {second,2,$.},
    {milli,3,none}
  ],
  Ret = lists:foldl(fun(Slot, AccIn) ->
    case AccIn of
      {error, Error} ->
        {error, Error};
      {ok, Date, Input} ->
        parse_slot(Slot, Input, Date)
    end
  end, {ok, #hty_date{}, Iso}, Slots),
  case Ret of
    {error, Error} ->
      {error, Error};
    {ok, Date, _} ->
      Date
  end.

parse_slot({Slot, Len, Delim}, In, Date) ->
  {Sublist, Rest} = lists:split(Len, In),
  case string:to_integer(Sublist) of
    {Int, []} ->
      case Rest of
        [Delim|Rest2] ->
          {ok, hty_date:with(Slot, Int, Date), Rest2};
        [Other|_] ->
          {error, {"Unexpected token ", Other}};
        [] ->
          case Delim of
            none ->
              {ok, hty_date:with(Slot, Int, Date), []};
            _ ->
              {error, {"expected delim ", Delim}}
          end
      end;
    {_, [Other|_]} ->
      {error, {"Unexpected token ", Other}}
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

daybreak(This) ->
  This#hty_date{
    hour=0,
    minute=0,
    second=0,
    milli=0
  }.

nightfall(This) ->
  This#hty_date{
    hour=23,
    minute=59,
    second=59,
    milli=999
  }.

year(This) ->
    This#hty_date.year.

month(This) ->
    This#hty_date.month.

day(This) ->
    This#hty_date.day.

tomorrow(This) ->
  Today = This#hty_date.day,
  Lastdayofmonth = calendar:last_day_of_the_month(
    This#hty_date.year,
    This#hty_date.month
  ),
  case Today < Lastdayofmonth of
    true -> This#hty_date{day=Today+1};
    false -> case This#hty_date.month == 12 of
      true -> This#hty_date{
        year=This#hty_date.year+1,
        month=1,
        day=1
      };
      false -> This#hty_date{
        month=This#hty_date.month+1,
        day=1
      }
    end
  end.
