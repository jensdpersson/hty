-module(hty_seqfile).
-export([incr/1]).

incr(File) ->
    LastId = case file:read_file(File) of
        {ok, LastJobId} ->
            {Integer, _} = string:to_integer(binary_to_list(LastJobId)),
            Integer;
        _ ->
            0
    end,
    NextId = integer_to_list(LastId + 1),
    case file:write_file(File, NextId) of
        ok -> 
            {ok, NextId};
        {error, Reason} ->
            {err, File, Reason}
    end.