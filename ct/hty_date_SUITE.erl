-module(hty_date_SUITE).
-compile(export_all).

all() -> [t0_test].

t0_test(_Cfg) ->
  Expected = "1970-01-01T00:00:00.095",
  Date = hty_date:parse(Expected),
  Actual = hty_date:format(Date),
  Expected = Actual.
