-module(hty_move_resource).
-record(hty_move_resource, {storagekey}).
-export([mount/2, handle/2]).

mount(Fspath, _Mc) ->
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
	        [<<"MOVE">>] -> 
            true;
	        _Other -> 
            erlang:display(hty_tx:req_headers(Htx)), false
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
              % Htx1 = hty_tx:echo(Destination, Htx),
              {ok, Fsdest} = hty_pathmapper:htx_to_fspath(Htx, This#hty_move_resource.storagekey, 
                  split(binary_to_list(Destination)),
                  lists:reverse(hty_tx:path_above(Htx))),
              case hty_fspath:move(Fspath, Fsdest) of
                ok ->
                    io:format("Moved OK"),
                    H = hty_tx:with([
                      {rsp_header, <<"location">>, Destination},
                    ok,
                    commit], Htx),
                    io:format("Moved OK2"), H;
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
  
  split(Path) -> 
    case string:split(Path, [$/], all) of 
        [""|Segs] -> Segs;
        Other -> Other
    end.
    
    
