-module(hty_lib).
-export([prefix/1]).
-callback prefix(Lib::tuple()) -> string().

prefix(Lib) ->
    (element(1, Lib)):prefix(Lib).