#!/usr/bin/env escript

main([]) ->
    L = [a,b,c],
    lists:foreach(fun(E) ->
			  io:format("Elm ~p~n", [E])
		  end, L).
