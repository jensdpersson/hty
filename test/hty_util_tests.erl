-module(hty_util_tests).

-export([keyappend_empty_test/0]).

keyappend_empty_test() ->
    [{key,[value]}] = hty_util:keyappend(key, value, []).