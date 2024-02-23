-module(hty_logger_resource).
-record(hty_logger_resource, {key, bound}).

-export([mount/2, new/2, handle/2]).

mount(Fspath, Mc) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["logger", Key|_] ->
      case hty_mounter:walk(Fspath, "logger", Mc) of
        {ok, Subs} ->
          {ok, new(Key, Subs)};
        {error, _} = Error ->
          Error
      end;
    ["logger"] ->
      {error, "Logger resource needs a parameter to use as the binding key"}
  end.

new(Key, Bound) ->
  #hty_logger_resource{key=Key, bound=Bound}.

handle(Htx, This) ->
  hty_tx:bind(This#hty_logger_resource.key, This#hty_logger_resource.bound, Htx).
