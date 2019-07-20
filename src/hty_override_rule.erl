-module(hty_override_rule).

-export([match/1]).

match(Walker) ->
  Fspath = Walker:fspath(),
    case lists:reverse(Fspath:parts()) of
        ["override"|RFilename] ->
            Filename = string:join(lists:reverse(RFilename), "."),
            {claim, {resource, {hty_override_resource:new(Filename, Fspath)}}};
        _ ->
            next
    end.
