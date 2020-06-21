-module(hty_getfile_resource).
-record(hty_getfile_resource, {storagekey}).
-export([mount/2, handle/2]).

mount(Fspath, _Mc) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["getfile", StorageKey|_] ->
      {ok, #hty_getfile_resource{storagekey=StorageKey}};
    _ ->
      {error, "getfile resource needs a storagekey parameter"}
  end.

handle(Htx, This) ->
    case hty_tx:method(Htx) of
	'GET' ->
	    Key = This#hty_getfile_resource.storagekey,
	    case hty_pathmapper:htx_to_fspath(Htx, Key) of
		{ok, Fspath} ->
		    case hty_fspath:isdir(Fspath) of
			true ->
			    hty_tx:forbidden(Htx);
			false ->
			    case hty_fspath:exists(Fspath) of
				true ->
				    hty_fileserver:serve(Htx, Fspath);
				false ->
				    hty_tx:not_found(Htx)
			    end
		    end;
		_ ->
		    erlang:display("Pathmapper not found")
	    end;
	_ ->
	    hty_tx:method_not_allowed(["GET"], Htx)
    end.
