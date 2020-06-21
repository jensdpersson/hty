-module(hty_mounter).
-record(hty_mounter, {prefixes=["hty"]}).
-export([walk/3, new/1, new/0]).

new() -> #hty_mounter{}.
new(Prefixes) when is_list(Prefixes) -> #hty_mounter{prefixes=Prefixes}.

mount(Fspath, Type, Mounter) ->
  Suffix = hty_fspath:ext(Fspath) ++ "_" ++ Type,
  Prefixes = Mounter#hty_mounter.prefixes,
  case mount2(Prefixes, Suffix, []) of
    {ok, Module} ->
      Module:mount(Fspath, Mounter);
    {error, Error, Fails} ->
      Msg = "Could not load module from any of",
      {error, {Msg, Fails, Error}}
  end.

mount2([Prefix|Prefixes], Suffix, Fails) ->
  Name = Prefix ++ "_" ++ Suffix,
  case code:ensure_loaded(list_to_atom(Name)) of
    {module, Module} ->
      {ok, Module};
    {error, Error} ->
      case Prefixes of
        [] ->
          {error, Error, [Name|Fails]};
        _ ->
          mount2(Prefixes, Suffix, [Name|Fails])
      end
  end.

walk(Fspath, Type, Mounter) ->
  Files = sort(hty_fspath:list(Fspath)),
  io:format("Walking ~p~n", [Files]),
  walk2(Files, Type, [], Mounter).

walk2([], _, Mounts, _) -> {ok, lists:reverse(Mounts)};
walk2([Fspath|Fspaths], Type, Mounts, Mounter) ->
  case mount(Fspath, Type, Mounter) of
    {ok, Mount} ->
      walk2(Fspaths, Type, [Mount|Mounts], Mounter);
    {error, _} = Error ->
      Error
  end.

sort(List) -> lists:sort(List).
