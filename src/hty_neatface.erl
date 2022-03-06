-module(hty_neatface).
-record(hty_neatface, {callback}).
-export([start_link/1]).

start_link(Callback) ->
    Pid = spawn_link(fun(Callback) loop(#hty_neatface{callback=Callback, state=[]}) end),
    Pid.

loop(This) ->
    Callback = This#hty_neatface.callback,
    receive 
	{append, Neatpath,  Data, Ref, ReplyTo} ->
	    {ok, Response, State1} = Callback:append(Neatpath, Data, State),
	    ReplyTo ! {ok, Response, Ref},
	    loop(Callback, State1);
	{query, Neatpath, ReplyTo} ->
	    {ok, Response, State1} = Callback:query(Neatpath, State),
	    ReplyTo ! {ok, Response, Ref},
	    loop(This#hty_neatpath{state=State1})
    end.
    
	   
