%@doc Ip is an ipv4 address as a 4-tuple of bytes, Port is an integer
-module(hty_server, [Ip, Port, Resource]).
-export([start/0, stop/0]).

start() ->
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
			      go -> loop_accept(Listen) 
			  end 
		  end,
	    Accepter = spawn(Fun),
	    ok = gen_tcp:controlling_process(Listen, Accepter),
	    register(process_key(), 
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
			

stop() ->
       Key = process_key(),
       Key ! stop,
       ok.

process_key() -> 
	      list_to_atom(atom_to_list(hty_server) ++ 
	      " running on port " ++ integer_to_list(Port)).
	      

loop_accept(Listen) ->
	case gen_tcp:accept(Listen) of
		{error, Error} -> log(Error);
		{ok, Socket} ->
			Handler = 
				spawn(
				  fun() -> 
						  receive 
							  go ->
								  Htx = hty_parser:parse(Socket),
								  Htx1 = Resource:handle(Htx),
									io:format("handle done~n"),
									%Htx2 = Htx1:flush(),									
								  hty_parser:respond(Socket, Htx1)
						  end 
				  end),
			ok = gen_tcp:controlling_process(Socket, Handler),
			Handler ! go,
			loop_accept(Listen)
	end.


log(Msg) -> io:format("~p~n", [Msg]).

