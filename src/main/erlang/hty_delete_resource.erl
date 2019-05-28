-module(hty_delete_resource).
-record(hty_delete_resource, {key}).
-export([mount/1, handle/2]).


mount(Fspath) ->
    case lists:reverse(Fspath:parts()) of
        ["delete", Key | _] ->
            {ok, #hty_delete_resource{key=Key}};
        _ ->
            {error, "delete resource requires a storage key parameter"}
    end.
    
    
handle(Htx, This) ->
  case Htx:method() of 
      'DELETE' ->
	    do_delete(Htx, This);
      _ ->
	    Htx:method_not_allowed(['DELETE'])
  end.

do_delete(Htx, This) ->
  Key = This#hty_delete_resource.key,
  case hty_pathmapper:htx_to_fspath(Htx, Key) of
    {ok, Fspath} ->
      case Fspath:exists() of
         true ->
            case Fspath:delete() of
                ok ->
                    Htx2 = Htx:ok(),
                    Htx2:commit();
	            {no, Why} -> 
	                Htx2 = Htx:server_error(Why),
	                Htx2:commit()
	        end;
		 false ->
		    Htx:not_found()
      end;
    _ ->
      erlang:display("Pathmapper not found"), Htx
  end.
