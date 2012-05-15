-module(hty_ctl).

-export([start/1, stop/0]).

-spec start(atom()) -> ok | {error, string()}.
%@spec start(atom()) -> ok | {error, string()}.
start([Path]) ->
	    Fscursor = hty_fs:cursor(Path),
	    case Fscursor:exists() of
	    	 false ->
		       {error, enoexist};
	    	 true -> 
		       case hty_main:start() of
		       	    ok -> 
			       hty_main:mount(Fscursor)
		       end
	    end.

stop() ->
       hty_main ! {stop, self()},
       receive
           stopping -> stopping
       after
           10000 -> timout
       end.
		      	    