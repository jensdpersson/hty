-module(hty_root_directive).

-export([new/0, add_sub/3, subs/2, handle/2]).

-record(hty_root_directive, {subs=[]}).

new() -> #hty_root_directive{}.

add_sub(Sub, _Role, This) ->
  #hty_root_directive{subs=[Sub|subs(This)]}.

subs(This) ->
  This#hty_root_directive.subs.

mount(Parent, _Fspath) ->
  Parent.

handle(Htx, This) ->
  Htx:dispatch(subs(This)).
