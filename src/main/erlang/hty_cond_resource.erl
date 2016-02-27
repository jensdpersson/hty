-module(hty_cond_resource).

-export([mount/1]).

mount(Fspath) ->
  case lists:reverse(Fspath:parts()) of
    ["cond", Expression] ->
      Conds = parse(Expression);
    ["cond"] ->
      Conds = ??
