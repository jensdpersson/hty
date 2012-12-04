-module(hty_ctl).

-export([start/1, stop/0]).



start([Path]) ->
	    Fscursor = hty_fs_cursor:new(atom_to_list(Path)),
	    case Fscursor:exists() of
	    	 false ->
		       {error, enoexist};
	    	 true -> 
		       case hty_main:start() of
		       	    ok -> 
			       hty_main:reload(Fscursor)
		       end
	    end.

stop() ->
       hty_main ! {stop, self()},
       receive
           stopping -> stopping
       after
           10000 -> timout
       end.
		      	    
