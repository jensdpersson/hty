-module(hty_vector, [Cmp, Cull, Ctor]).

-export([filter/2]).
	
filter(Olds, News) ->
	f2(Olds, News, [], []).

f([], [], Rs, _Nso) -> Rs;
f([], [N|Ns], Rs, Nso) ->
      f([], Ns, [Ctor(N)|Rs], Nso);
f([O|Os], [], Rs, Nso) ->
	  Cull(O),
	  f(Os, Nso, Rs, Nso);
f([O|Os], [N|Ns], Rs, Nso) ->
	  case Cmp(O,N) of
	     true -> 
	     	  f(Os, Ns, [O|Rs], Nso);
	     false ->
	          f([O|Os], Ns, Rs, Nso)
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
       