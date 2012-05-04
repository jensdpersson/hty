%@doc Ip is an ipv4 address as a 4-tuple of bytes, Port is an integer
-module(hty_server, [Ip, Port, Router]).
-export([start/0, stop/0]).

start() ->
    io:format("Trying to listen on ~p:~p", [Ip,Port]),
    case gen_tcp:listen(Port, [list, {packet, 0}, {active, once}, {ip, Ip}, {reuseaddr, true}]) of
        {error, Error} ->
            io:format(", failed : ~p~n", [Error]),
            {error, Error};
        {ok, Listen} ->
            io:format(", ok~n"),
	    Fun = fun() -> 
			  receive 
			      go -> loop_accept(Listen) 
			  end 
		  end,
	    Accepter = spawn(Fun),
	    ok = gen_tcp:controlling_process(Listen, Accepter),
	    Accepter ! go,
            ok
    end.


stop() -> no.

loop_accept(Listen) ->
    case gen_tcp:accept(Listen) of
	{error, Error} -> log(Error);
	{ok, Socket} ->
	    Handler = spawn(fun() -> 
				    receive {route, Route} ->
					    Req = hty_parser:parse(Socket),
					    Rsp = Route:handle(Req),
					    hty_parser:respond(Socket, Rsp)
				    end 
			    end),
	    ok = gen_tcp:controlling_process(Socket, Handler),
	    Router ! {request, Handler},
	    loop_accept(Listen)
    end.


log(Msg) -> io:format("~p~n", [Msg]).

