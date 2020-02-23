-module(hty_glob_resource).
-record(hty_glob_resource, {key}).
-export([mount/1, handle/2]).

mount(Fspath) ->
    case lists:reverse(hty_fspath:parts(Fspath)) of
        ["glob", Key | _] ->
            {ok, #hty_glob_resource{key=Key}};
        _ ->
            {error, "glob resource requires a storage key parameter"}
    end.


handle(Htx, This) ->
  case hty_tx:method(Htx) of
      'GET' ->
	    do_get(Htx, This);
      'POST' ->
        do_post(Htx, This);
      _ ->
	    hty_tx:method_not_allowed(['GET', 'POST'], Htx)
  end.

do_get(Htx, This) ->
  Key = This#hty_glob_resource.key,
  case hty_pathmapper:htx_to_fspath(Htx, Key) of
    {ok, Fspath} ->
      case hty_fspath:exists(Fspath) of
         true ->
            case hty_tx:path_below(Htx) of
                [] ->
                    respond(Htx, []);
                Globpath ->
                    Results = hty_glob:find(Globpath, Fspath),
                    respond(Htx, Results)
            end;
		 false ->
		    hty_tx:not_found(Htx)
      end;
    _ ->
      erlang:display("Pathmapper not found"), Htx
  end.
  
  
do_post(Htx, This) ->
    FormSchema = [root, glob], 
    FormSchema = [
		{<<"root">>, [], [{occurs, 1, 1}]},
		{<<"glob">>, [], [{occurs, 1, 1}]}
	], 
	hty_tx:recvform(FormSchema, fun(Form, Htx2) -> 
    	{_, [Root], _} = lists:keyfind(<<"root">>, 1, Form),
	    {_, [Glob], _} = lists:keyfind(<<"glob">>, 1, Form),
	    Proc = hty_proc:new(fun(_Progressor) -> hty_glob:find(Glob, Root) end),
	    hty_tx:see_other(Proc#hty_proc.location, Htx)
	end, Htx).
	
local_path(Urlpath, This) -> notyet.
    
  
respond(Htx, Results) ->
    Htx0 = hty_tx:rsp_header("Content-Type", "text/urilist", Htx),
    Pathlist = lists:map(fun(Elem) -> 
        string:join(Elem, "/")
    end, Results),
    Body = string:join(Pathlist, "\r\n"),
    Htx1 = hty_tx:echo(Body, Htx0),
    Htx2 = hty_tx:ok(Htx1),
    hty_tx:commit(Htx2).
