-module(hty_vector, [Cmp, Cull, Ctor]).

-export([filter/2]).

filter(Olds, News) ->
	     f(Olds, News, [], News).

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


       