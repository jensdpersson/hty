-module(hty_ctl).

-export([start/1, stop/0]).

start([Path]) when is_atom(Path) ->
    start([atom_to_list(Path)]);
start([Path]) ->
    Fspath = hty_fspath:new(Path),
    case Fspath:exists() of
	false ->
	    {error, enoexist};
	true ->
	    case hty_main:start() of
		ok ->
		    hty_main:reload(Fspath)
	    end
    end.

stop() ->
    hty_main ! {stop, self()},
    receive
	stopping -> ok
    after
	10000 -> timeout
    end.
