-module(hty_crash_rule).

-export([match/2]).

match(Fspath, _Rules) ->
    io:format("Mounting crash in ~p~n", [Fspath:ext()]),
    case Fspath:ext() of
        "crash" ->
            {claim, {resource, nonexisting_resource}};
        _ ->
            next
    end. 