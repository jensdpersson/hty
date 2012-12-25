-module(hty_vector, [Cmp, Cull, Ctor]).

-export([update/2, update/3]).
	
update(Olds, Remove, Add) ->
	Olds1 = remove([], Olds, Remove, Remove),
	add(Add, Olds1). 

update(Olds, News) ->
	f2(Olds, News, [], []).

remove(Os1, [], _, _) ->
	lists:reverse(Os1);
remove(Os1, [O|Os], [], Rs0) ->
	remove([O|Os1], Os, Rs0, Rs0);
remove(Os1, [O|Os], [R|Rs], Rs0) ->
	case Cmp(O,R) of
		true ->
			Cull(O),
			remove(Os1, Os, Rs, Rs0);
		false ->
			remove(Os1, [O|Os], Rs, Rs0)
	end.
			
			
f2([O|Os], [N|Ns], Ks, N1s) ->
	case Cmp(O,N) of
	    true -> 
			f2(Os, Ns, [O|Ks], N1s);
		false ->
			f2([O|Os], Ns, Ks, [N|N1s])
	end;
f2([], Ns, Ks, N1s) ->
	add(Ns, add(N1s, Ks));
f2([O|Os], [], Ks, N1s) ->
	Cull(O),
	f2(Os, N1s, Ks, []).

add([], L) -> L;
add([N|Ns], L) -> 
	add(Ns, [Ctor(N)|L]).
       

	
