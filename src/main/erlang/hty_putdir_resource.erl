-module(hty_putdir_resource).
-record(hty_putdir_resource, {storagekey}).

-export([mount/1, handle/2]).

mount(Fspath) ->
  case lists:reverse(Fspath:parts()) of
    ["putdir", StorageKey|_] ->
      {ok, #hty_putdir_resource{storagekey=StorageKey}};
    _ ->
      {error, "putdir resource needs a storagekey parameter"}
  end.

handle(Htx, This) ->
    case Htx:method() of
	'PUT' ->
	    case Htx:req_header("Content-Type") of
	        [<<"text/uri-list">>] -> 
        	    Key = This#hty_putdir_resource.storagekey,
        	    case hty_pathmapper:htx_to_fspath(Htx, Key) of
            		{ok, Fspath} ->
            		    Fsparent = Fspath:parent(),
            		    case Fsparent:isdir() of
            			false ->
            			    Htx:not_found();
            			true ->
            			    case Fspath:exists() of
            				true ->
            				    Htx:conflict();
            				false ->
            				    case Fspath:mkdir() of
            				        ok ->
            				            (Htx:created()):commit();
            				        error ->
            				            Htx:server_error()
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
	    Htx:method_not_allowed(['GET'])
    end.
