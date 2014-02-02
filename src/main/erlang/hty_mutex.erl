-module(hty_mutex).

-export([
		 start/0,
		 stop/0,
		 lock/4,
		 release/1
		 ]).

start() ->
	Pid = spawn(fun() -> 
						Tid = ets:new(hty_mutex, [private]),
						loop(Tid, 0) 
				end),
	register(hty_mutex, Pid),
	ok.

stop() ->
	hty_mutex ! {stop, self()},
	receive
		ok ->
			ok
	end.		


-spec lock(term(), for_reading|for_writing, wait, Millis::integer|forever) -> any().
lock(SomeTerm, ReadOrWrite, wait, Wait) ->
	Type = case ReadOrWrite of
			   for_reading -> read;
			   for_writing -> write
		   end,
	hty_mutex ! {Type, SomeTerm, self()},
	Timeout = case Wait of
				  forever ->
					  infinity;
				  _ ->
					  Wait
			  end,
	receive
		{hty_mutex, _Token} = M ->
			M
	after 
		Timeout ->
			{timeout, SomeTerm, ReadOrWrite, Wait}
	end.

release({hty_mutex, Token, _}) ->
	hty_mutex ! {unlock, Token},
	ok.

loop(Table, _LastToken) ->
	receive
		{Type, MutexID, ReplyTo} ->
			case ets:lookup(Table, MutexID) of
				{queue, MutexID, []} -> 
					ets:insert(Table, {queue, MutexID, [{Type, ReplyTo}]}),
					ReplyTo ! {};
				{queue, MutexID, Queue} ->
					ets:insert(Table, {queue, MutexID, [{Type, ReplyTo}|Queue]})
			end;
		{stop, ReplyTo} ->
			ReplyTo ! ok
	end.
			
			
			
			
			
			
			
			
			