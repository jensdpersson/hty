-module(hty_getfile_resource).
-record(hty_getfile_resource, {storagekey}).
-export([mount/1, handle/2]).

mount(Fspath) ->
  case lists:reverse(Fspath:parts()) of
    ["getfile", StorageKey|_] ->
      {ok, #hty_getfile_resource{storagekey=StorageKey}};
    _ ->
      {error, "getfile resource needs a storagekey parameter"}
  end.

handle(Htx, This) ->
    case Htx:method() of
	'GET' ->
	    Key = This#hty_getfile_resource.storagekey,
	    case hty_pathmapper:htx_to_fspath(Htx, Key) of
		{ok, Fspath} ->
		    case Fspath:isdir() of
			true ->
			    Htx:forbidden();
			false ->
			    case Fspath:exists() of
				true ->
				    hty_fileserver:serve(Htx, Fspath);
				false ->
				    Htx:not_found()
			    end
		    end;
		_ ->
		    erlang:display("Pathmapper not found")
	    end;
	_ ->
	    Htx:method_not_allowed(['GET'])
    end.
