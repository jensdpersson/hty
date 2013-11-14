-module(hty_override_rule).

-export([match/2]).

match(Fspath, _Rules) ->
    case lists:reverse(Fspath:parts()) of
        ["override"|RFilename] ->
            Filename = string:join(lists:reverse(RFilename), "."),
            {claim, {resource, {hty_override_resource:new(Filename, Fspath)}}};
        _ ->
            next
    end.
            