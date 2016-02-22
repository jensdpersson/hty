-module(hty_mounter).

-export([walk/2]).

mount(Fspath, Type) ->
  Prefixes = ["hty_"],
  Suffix = Fspath:ext() ++ "_" ++ Type,
  case mount2(Prefixes, Suffix, []) of
    {ok, Module} ->
      Module:mount(Fspath);
    {error, Error, Fails} ->
      Msg = "Could not load module from any of",
      {error, {Msg, Fails, Error}}
  end.

mount2([Prefix|Prefixes], Suffix, Fails) ->
  Name = Prefix ++ Suffix,
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

walk(Fspath, Type) ->
  walk2(Fspath:list(), Type, []).

walk2([], _, Mounts) -> {ok, Mounts};
walk2([Fspath|Fspaths], Type, Mounts) ->
  case mount(Fspath, Type) of
    {ok, Mount} ->
      walk2(Fspaths, Type, [Mount|Mounts]);
    {error, _} = Error ->
      Error
  end.
%append(Dict, Key, Value) ->
%  case dict:is_key(Key, Dict) of
%    true ->
%      dict:append_list(Key, Value, Dict);
%    false ->
%      dict:store(Key, [Value], Dict)
%  end.
