-module(hty_timeseries_resource).

-export([new/1, handle/2]).

-record(hty_timeseries_resource, {folder}).

new(Folder) ->
  #hty_timeseries_resource{folder=Folder}.

handle(Htx, This) ->
    case Htx:method() of
	     'GET' ->
	        Filter = case Htx:path_below() of
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
          Folder1 = year(Now, Folder),
          not_implemented
    end.

year(Date, Folder) ->
  Year = Folder:subpath([Date:year()]),
  case Year:exists() of
    false ->
      Year:mkdir();
    true ->
      ok
  end,
  month(Date, Year).

month(Date, Folder) ->
  Month = Folder:subpath([Date:month()]),
  case Month:exists() of
    false ->
      Month:mkdir();
    true ->
      ok
  end,
  day(Date, Month).

day(Date, Folder) ->
  Day = Folder:subpath([Date:day()]),
  case Day:exists() of
    false ->
      Day:mkdir();
    true ->
      ok
  end,
  Day.
