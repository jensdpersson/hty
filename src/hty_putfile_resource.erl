%% Author: jens
-module(hty_putfile_resource).
-record(this, {key}).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([mount/2, handle/2, new/1]).


mount(Fspath, _Mc) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["putfile", Key | _] ->
      {ok, new(Key)};
    _ ->
      {error, "putfile resource requires a storage key parameter"}
  end.

new(Key) ->
    #this{key=Key}.

handle(Htx, This) ->
  case hty_tx:method(Htx) of
      'PUT' ->
	  do_put(Htx, This);
      _ ->
	  hty_tx:method_not_allowed(["PUT"], Htx)
  end.

do_put(Htx, This) ->
  Key = This#this.key,
  case hty_pathmapper:htx_to_fspath(Htx, Key) of
    {ok, Fspath} ->
      case hty_fspath:isdir(Fspath) of
        true ->
          hty_tx:forbidden(Htx);
        false ->
	      Fsparent = hty_fspath:parent(Fspath),
	      case hty_fspath:exists(Fsparent) of
	      	   true ->
		      Htx1 = hty_fspath:recv([], Htx, Fspath),
		      hty_tx:commit(hty_tx:ok(Htx1));
		   false ->
		      hty_tx:not_found(Htx)
	      end
      end;
    _ ->
      erlang:display("Pathmapper not found")
  end.
