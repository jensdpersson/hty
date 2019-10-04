-module(hty_delete_resource).
-record(hty_delete_resource, {key}).
-export([mount/1, handle/2]).


mount(Fspath) ->
    case lists:reverse(hty_fspath:parts(Fspath)) of
        ["delete", Key | _] ->
            {ok, #hty_delete_resource{key=Key}};
        _ ->
            {error, "delete resource requires a storage key parameter"}
    end.


handle(Htx, This) ->
  case hty_tx:method(Htx) of
      'DELETE' ->
	    do_delete(Htx, This);
      _ ->
	    hty_tx:method_not_allowed(['DELETE'], Htx)
  end.

do_delete(Htx, This) ->
  Key = This#hty_delete_resource.key,
  case hty_pathmapper:htx_to_fspath(Htx, Key) of
    {ok, Fspath} ->
      case hty_fspath:exists(Fspath) of
         true ->
            case hty_fspath:delete(Fspath) of
                ok ->
                    hty_tx:commit(hty_tx:ok(Htx));
	            {no, Why} ->
	                hty_tx:commit(hty_tx:server_error(Why, Htx))
	        end;
		 false ->
		    hty_tx:not_found(Htx)
      end;
    _ ->
      erlang:display("Pathmapper not found"), Htx
  end.
