%@doc Ip is an ipv4 address as a 4-tuple of bytes, Port is an integer
-module(hty_server).
-export([new/3, start/1, stop/1]).

-record(hty_server, {ip, port, resource}).

new(Ip, Port, Resource) ->
    #hty_server{ip=Ip,port=Port,resource=Resource}.

start(This) ->
    Ip = This#hty_server.ip,
    Port = This#hty_server.port,
    Resource = This#hty_server.resource,
    io:format("Trying to listen on ~p:~p", [Ip,Port]),
    case gen_tcp:listen(Port, [list, 
			       {packet, 0}, 
			       {active, once}, 
			       {ip, Ip}, 
			       {reuseaddr, true},
			       binary]) of
        {error, Error} ->
            io:format(", failed : ~p~n", [Error]),
            {error, Error};
        {ok, Listen} ->
            io:format(", ok~n"),
	    Fun = fun() -> 
			  receive 
			      go -> loop_accept(Listen, Resource) 
			  end 
		  end,
	    Accepter = spawn(Fun),
	    ok = gen_tcp:controlling_process(Listen, Accepter),
	    register(process_key(This), 
		     spawn(fun() -> 
				   loop_control([])
			   end)
		    ),
	    Accepter ! go,
            ok
    end.

loop_control(Sites) ->
    receive
	stop -> ok;
	{sites, Sites} -> loop_control(Sites)
    end.

stop(This) ->
       Key = process_key(This),
       Key ! stop,
       ok.

process_key(This) -> 
	      list_to_atom(atom_to_list(hty_server) ++ 
	      " running on port " ++ integer_to_list(This#hty_server.port)).
	      

loop_accept(Listen, Resource) ->
    case gen_tcp:accept(Listen) of
	{error, Error} -> 
	    error_logger:format("Accept failed ~p~n", [Error]);
	{ok, Socket} ->
	    Handler = spawn(fun() -> 
				    receive 
					go ->
					    Htx = hty_parser:parse(Socket),
					    Htx1 = Resource:handle(Htx),
					    hty_parser:respond(Socket, Htx1)
				    end 
			    end),
	    case gen_tcp:controlling_process(Socket, Handler) of
		ok ->
		    Handler ! go;
		{error, Error} ->
		    error_logger:format("Controlling process change failed ~p~n", [Error])		
	    end,
	    loop_accept(Listen, Resource)
    end.



