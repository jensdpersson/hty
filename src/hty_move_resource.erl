-module(hty_move_resource).
-record(hty_move_resource, {storagekey}).
-export([mount/1, handle/2]).

mount(Fspath) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["move", StorageKey|_] ->
      {ok, #hty_move_resource{storagekey=StorageKey}};
    _ ->
      {error, "move resource needs a storagekey parameter"}
  end.

handle(Htx, This) ->
  
  OkMethod = case hty_tx:method(Htx) of
	'MOVE' -> true;
	'GET' -> false;
	_ ->
	    case hty_tx:req_header("X-HTTP-Method-Override", Htx) of
	        [<<"MOVE">>] -> true;
	        Other -> erlang:display(hty_tx:req_headers(Htx)), false
	    end
  end,
  
  case OkMethod of
    false ->
      hty_tx:method_not_allowed(["MOVE"], Htx);
    true -> 
	  case hty_tx:req_header("Destination", Htx) of
	    [] ->
	      hty_tx:bad_request(Htx);
	    [Destination|_] ->
	      case hty_pathmapper:htx_to_fspath(Htx, This#hty_move_resource.storagekey) of
            {ok, Fspath} ->
              io:format("Moving ~p to ~p~n", [Fspath, Destination]),
              Htx1 = hty_tx:echo(Destination, Htx),
              {ok, Fsdest} = hty_pathmapper:htx_to_fspath(Htx, This#hty_move_resource.storagekey, 
                  [binary_to_list(Destination)]),
              case hty_fspath:move(Fspath, Fsdest) of
                ok -> hty_tx:commit(hty_tx:ok(Htx1));
                {error, enoent} -> hty_tx:not_found(Htx);
                {error, eaccess} -> hty_tx:forbidden(Htx);
                {error, Error} -> 
                   io:format("MoveError ~p~n", [Error]),    
                   hty_tx:server_error(Error, Htx)
              end;
            {no, Error} ->
              io:format("Map Error ~p ~n", [Error]),
              hty_tx:server_error(Error, Htx)        
          end
      end
  end.
    
    
