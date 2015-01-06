-module(hty_timeseries_resource_tests).

-export([
  add_entry_test/0
]).

add_entry_test() ->
  Dut = dut(),
  Htx = hty_tx_factory:post(<<"2014-11-12">>,
    <<"start=someproj">>,
    <<"text/x-www-form-urlencoded">>),
  Htx1 = Htx:dispatch(Dut),
  {200, _} = Htx1.

dut() ->
  %{A,B,C} = now(),
  %Path = integer_to_list(A) ++ integer_to_list(B) ++ integer_to_list(C),
  Fsc = hty_fs_cursor:new("target/" ++ hty_log:tstamp() ++ ".tmp"),
  Fsc:mkdir(),
  hty_timeseries_resource:new(Fsc).
