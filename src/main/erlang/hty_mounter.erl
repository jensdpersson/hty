-module(hty_mounter).

-export([mount/2]).

mount(Parent, Fspath) ->
  Name = "hty_" ++ Fspath:ext() ++ "_directive",
  Module = list_to_atom(Name),
  Directive = Module:new(Fspath),
  Fun = fun(Fspath2, Directive2) ->
            mount(Directive2, Fspath2)
        end,
  Acc = Directive,
  List = Fspath:list(),
  Directive1 = lists:foldl(Fun, Acc, List),
  Directive1:add_to_parent(Parent, Fspath).
