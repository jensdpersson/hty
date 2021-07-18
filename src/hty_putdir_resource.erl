-module(hty_putdir_resource).
-record(hty_putdir_resource, {storagekey}).

-export([mount/2, handle/2]).

mount(Fspath, _Mc) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["putdir", StorageKey|_] ->
      {ok, #hty_putdir_resource{storagekey=StorageKey}};
    _ ->
      {error, "putdir resource needs a storagekey parameter"}
  end.

handle(Htx, This) ->
    case hty_tx:method(Htx) of
	'PUT' ->
	    case hty_tx:req_header("Content-Type", Htx) of
	        [<<"text/uri-list">>] ->
        	    Key = This#hty_putdir_resource.storagekey,
        	    case hty_pathmapper:htx_to_fspath(Htx, Key) of
            		{ok, Fspath} ->
            		    Fsparent = hty_fspath:parent(Fspath),
            		    case hty_fspath:isdir(Fsparent) of
            			false ->
            			    hty_tx:not_found(Htx);
            			true ->
            			    case hty_fspath:exists(Fspath) of
            				true ->
            				    hty_tx:conflict(Htx);
            				false ->
            				    case hty_fspath:mkdir(Fspath) of
            				        ok ->
            				            hty_tx:commit(hty_tx:created(Htx));
            				        error ->
            				            hty_tx:server_error(Htx)
            				    end
            			    end
            		    end;
            		_ ->
		                erlang:display("Pathmapper not found")   , Htx
                end;
            OtherMime ->
        		io:format("OtherMime, wont mkdir [~p]~n", [OtherMime]), Htx
        end;
	_ ->
	    hty_tx:method_not_allowed(["PUT"], Htx)
    end.
