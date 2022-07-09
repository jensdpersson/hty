-module(hty_copy_resource).
-record(hty_copy_resource, {storagekey}).
-export([mount/2, handle/2]).

mount(Fspath, _Mc) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["copy", StorageKey|_] ->
      {ok, #hty_copy_resource{storagekey=StorageKey}};
    _ ->
      {error, "copy resource needs a storagekey parameter"}
  end.

handle(Htx, This) ->
  
  OkMethod = case hty_tx:method(Htx) of
	'COPY' -> true;
	'GET' -> false;
	_ ->
	    case hty_tx:req_header("X-HTTP-Method-Override", Htx) of
	        [<<"COPY">>] -> 
            true;
	        _Other -> 
            erlang:display(hty_tx:req_headers(Htx)), false
	    end
  end,
  
  case OkMethod of
    false ->
      hty_tx:method_not_allowed(["COPY"], Htx);
    true -> 
	  case hty_tx:req_header("Destination", Htx) of
	    [] ->
	      hty_tx:bad_request(Htx);
	    [Destination|_] ->
	      case hty_pathmapper:htx_to_fspath(Htx, This#hty_copy_resource.storagekey) of
            {ok, Fspath} ->
              io:format("Copying ~p to ~p~n", [Fspath, Destination]),
              % Htx1 = hty_tx:echo(Destination, Htx),
              {ok, Fsdest} = hty_pathmapper:htx_to_fspath(Htx, This#hty_copy_resource.storagekey, 
                  split(binary_to_list(Destination)),
                  lists:reverse(hty_tx:path_above(Htx))),
              case hty_fspath:copy(Fspath, Fsdest) of
                {ok, Bytes} ->
                    io:format("Copied ~p bytes OK", [Bytes]),
                    H = hty_tx:with([
                      {rsp_header, <<"location">>, Destination},
                    ok,
                    commit], Htx),
                    H;
                ok ->
                    io:format("Copied folder OK"),
                    hty_tx:with([
                      {rsp_header, <<"location">>, Destination},
                      ok,
                      commit], Htx);
                {error, eexist} -> 
                    io:format("File exists ~p ~n", [Destination]),
                    hty_tx:with([
                        precondition_failed,
                        {echo, "File exists " ++ Destination}
                    ], Htx);
                {error, enoent} -> hty_tx:not_found(Htx);
                {error, eaccess} -> hty_tx:forbidden(Htx);
                {error, Error} -> 
                   io:format("CopyError ~p~n", [Error]),    
                   hty_tx:server_error(Error, Htx)
              end;
            {no, Error} ->
              erlang:display(Error),
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
    
    
