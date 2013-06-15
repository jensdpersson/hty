-module(hty_lock).

-export([read/2, write/2]).

-spec read(atom(), fun()) -> ok | no.

read(Lockname, Fun) ->
    run(read, Lockname, Fun).

write(Lockname, Fun) ->
    run(write, Lockname, Fun).

%% Internal
run(Locktype, Lockname, Fun) ->
    Lock = get_lock(Lockname),
    Lock ! {Locktype, self()},
    receive 
	ok ->
	    Fun()
    end.

get_lock(Lockname) ->
    Lockname1 = list_to_atom("hty_lock_" ++ atom_to_list(Lockname)),
    case whereis(Lockname1) of
	undefined ->
	    Actor = spawn(fun() -> loop([]) end),
	    register(Lockname1, Actor);
	Pid ->
	    Pid
    end.

loop(Locks) ->
    receive
	bob ->
	    loop(Locks);
	_  ->
	    ok
    end.

		  
		   
