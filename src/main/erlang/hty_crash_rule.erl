-module(hty_crash_rule).

-export([match/1]).

match(Walker) ->
    Fspath = Walker:fspath(),
    io:format("Mounting crash in ~p~n", [Fspath:ext()]),
    case Fspath:ext() of
        "crash" ->
            {claim, {resource, nonexisting_resource}};
        _ ->
            next
    end.
