-module(hty_ctl).

-export([start/1, stop/0]).

start([Path]) when is_atom(Path) ->
    start([atom_to_list(Path)]);
start([Path]) ->
    Fscursor = hty_fs_cursor:new(Path),
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
	10000 -> timeout
    end.
		      	    
