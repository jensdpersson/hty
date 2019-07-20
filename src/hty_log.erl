-module(hty_log).

-export([tstamp/0, today/0, iso8601/1, log/1]).

%doc Format a timestamp
tstamp() ->
    iso8601(erlang:localtime()).

today() ->
    {{Y,M,D},_} = erlang:localtime(),
    pad(Y) ++ "-" ++
    pad(M) ++ "-" ++
    pad(D).

iso8601(DateTime) ->
	{{Y,M,D},{H,Mi,S}} = DateTime,
	pad(Y) ++ "-" ++
    pad(M) ++ "-" ++
    pad(D) ++ "T" ++
    pad(H) ++ ":" ++
    pad(Mi) ++ ":" ++
    pad(S).

log(Msg) -> io:format("~p~n", [Msg]).

pad(Int) ->
	case Int > 9 of
		true -> integer_to_list(Int);
		false -> "0" ++ integer_to_list(Int)
	end.
