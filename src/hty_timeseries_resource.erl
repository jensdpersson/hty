-module(hty_timeseries_resource).

-export([new/1, handle/2]).

-record(hty_timeseries_resource, {folder}).

new(Folder) ->
  #hty_timeseries_resource{folder=Folder}.

handle(Htx, This) ->
    case hty_tx:method(Htx) of
	     'GET' ->
	        _Filter = case hty_tx:path_below(Htx) of
            [From, To] ->
              {hty_date:parse(From), hty_date:parse(To)};
            [From] ->
              {hty_date:parse(From)};
            [] ->
              none
          end;
	     'POST'->
	        Now = hty_date:now(),
          Folder = This#hty_timeseries_resource.folder,
          _Folder1 = year(Now, Folder),
          hty_tx:server_error(not_implemented, Htx)
    end.

year(Date, Folder) ->
  Year = hty_fspath:subpath([integer_to_list(hty_date:year(Date))], Folder),
  case hty_fspath:exists(Year) of
    false ->
      hty_fspath:mkdir(Year);
    true ->
      ok
  end,
  month(Date, Year).

month(Date, Folder) ->
  Month = hty_fspath:subpath([integer_to_list(hty_date:month(Date))], Folder),
  case hty_fspath:exists(Month) of
    false ->
      hty_fspath:mkdir(Month);
    true ->
      ok
  end,
  day(Date, Month).

day(Date, Folder) ->
  Day = hty_fspath:subpath([integer_to_list(hty_date:day(Date))], Folder),
  case hty_fspath:exists(Day) of
    false ->
      hty_fspath:mkdir(Day);
    true ->
      ok
  end,
  Day.
