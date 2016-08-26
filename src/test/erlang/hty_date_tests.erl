-module(hty_date_tests).

-export([t0_test/0]).

t0_test() ->
  Expected = "1970-01-01T00:00:00.000",
  Date = hty_date:parse(Expected),
  Actual = Date:format(),
  Expected = Actual.
