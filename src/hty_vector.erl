-module(hty_vector).

-export([new/3, update/3, update/4]).

-record(hty_vector, {cmp, cull, ctor}).

new(Cmp, Cull, Ctor) ->
    #hty_vector{cmp=Cmp, cull=Cull, ctor=Ctor}.


update(Olds, Remove, Add, This) ->
	Olds1 = remove([], Olds, Remove, Remove, This),
	Olds2 = lists:reverse(Olds1),
	News = add(Add, Olds2, This),
	lists:reverse(News).

update(Olds, News, This) ->
    f2(Olds, News, [], [], This).

remove(Os1, [], _, _, _This) ->
    lists:reverse(Os1);
remove(Os1, [O|Os], [], Rs0, This) ->
    remove([O|Os1], Os, Rs0, Rs0, This);
remove(Os1, [O|Os], [R|Rs], Rs0, This) ->
    Cmp = This#hty_vector.cmp,
    Cull = This#hty_vector.cull,
    case Cmp(O,R) of
	true ->
	    Cull(O),
	    remove(Os1, Os, Rs, Rs0, This);
	false ->
	    remove(Os1, [O|Os], Rs, Rs0, This)
    end.


f2([O|Os], [N|Ns], Ks, N1s, This) ->
    Cmp = This#hty_vector.cmp,
    case Cmp(O,N) of
	true ->
	    f2(Os, Ns, [O|Ks], N1s, This);
	false ->
	    f2([O|Os], Ns, Ks, [N|N1s], This)
    end;
f2([], Ns, Ks, N1s, This) ->
    add(Ns, add(N1s, Ks, This), This);
f2([O|Os], [], Ks, N1s, This) ->
    Cull = This#hty_vector.cull,
    Cull(O),
    f2(Os, N1s, Ks, [], This).

add([], L, _) -> L;
add([N|Ns], L, This) ->
    Ctor = This#hty_vector.ctor,
    add(Ns, [Ctor(N)|L], This).
