%@doc Ip is an ipv4 address as a 4-tuple of bytes, Port is an integer
-module(hty_server).
-export([new/4, start/1, stop/1]).
-export([protocol/1, address/1, port/1, root/1]).

-record(hty_server, {proto, ip, port, resource}).

new(Proto, Ip, Port, Resource) ->
    #hty_server{proto=Proto, ip=Ip,port=Port,resource=Resource}.

protocol(This) ->
  This#hty_server.proto.

address(This) ->
  This#hty_server.ip.

port(This) ->
  This#hty_server.port.

root(This) ->
  This#hty_server.resource.

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
