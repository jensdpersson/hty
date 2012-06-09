-module(hty_log).

-export([tstamp/0, log/1]).

%doc Format a timestamp
tstamp() ->
    {{Y,M,D},{H,Mi,S}} = erlang:localtime(),
    integer_to_list(Y) ++ "-" ++
    integer_to_list(M) ++ "-" ++
    integer_to_list(D) ++ "T" ++
    integer_to_list(H) ++ ":" ++
    integer_to_list(Mi) ++ ":" ++
    integer_to_list(S).

log(Msg) -> io:format("~p~n", [Msg]).