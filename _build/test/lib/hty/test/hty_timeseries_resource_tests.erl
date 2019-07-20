-module(hty_timeseries_resource_tests).

-export([
  add_entry_test/0
]).

add_entry_test() ->
  Dut = dut(),
  Htx = hty_tx_factory:post(<<"2014-11-12">>,
    <<"start=someproj">>,
    <<"text/x-www-form-urlencoded">>),
  Htx1 = hty_tx:dispatch(Dut, Htx).
  % for now, resource is not implemented
  %{200, _} = hty_tx:status(Htx1).

dut() ->
  %{A,B,C} = now(),
  %Path = integer_to_list(A) ++ integer_to_list(B) ++ integer_to_list(C),
  Fspath = hty_fspath:new("target/" ++ hty_log:tstamp() ++ ".tmp"),
  hty_fspath:mkdir(Fspath),
  hty_timeseries_resource:new(Fspath).
