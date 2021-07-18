-module(hty_glob_resource).
-record(hty_glob_resource, {
    target_store_key,
    proc_store_key,
    location_prefix}).
-export([mount/2, handle/2]).

mount(Fspath, _Mc) ->
    case lists:reverse(hty_fspath:parts(Fspath)) of
        ["glob", ProcStore, TargetStore | Rest] ->
            %Lastidfile = hty_fspath:subpath(["lastid"], Fspath),
            Baseurlvar = case Rest of
                [Baseurlvar0|_] -> {key, Baseurlvar0};
                _ -> {val, "/"}
            end,
            {ok, #hty_glob_resource{
                target_store_key=TargetStore, 
                proc_store_key=ProcStore,
                location_prefix={key, Baseurlvar}
            }};
        ["glob"|_] ->
            case hty_fspath:isfile(Fspath) of
                true ->
                    case hty_fspath:load(Fspath) of
                        {ok, Binary} ->
                            {ok, parse_config(Binary)};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                false ->
                    {error, "no path params and no file content"}
            end;
        _ ->
            {error, "glob resource requires a storage key parameter"}
    end.

parse_config(Binary) ->
    {ok, _, Props} = hty_formurlencoded_spaf:parse(Binary, q0),
    lists:foldl(fun(E, A) -> 
        case E of
            {kv, <<"target_store_key">>, V} ->
                A#hty_glob_resource{target_store_key=binary_to_list(V)};
            {kv, <<"proc_store_key">>, V} ->
                A#hty_glob_resource{proc_store_key=binary_to_list(V)};
            {kv, <<"location_prefix_key">>, V} ->
                A#hty_glob_resource{location_prefix={key, binary_to_list(V)}};
            {kv, <<"location_prefix">>, V} ->
                A#hty_glob_resource{location_prefix={val, binary_to_list(V)}}
        end
    end, #hty_glob_resource{}, Props).

handle(Htx, This) ->
  TargetStoreKey = This#hty_glob_resource.target_store_key,
  case hty_pathmapper:htx_to_fspath(Htx, TargetStoreKey) of
    {ok, TargetStoreRoot} ->
        case hty_fspath:exists(TargetStoreRoot) of
            true ->
              case hty_tx:method(Htx) of
                  'GET' ->
            	    do_get(Htx, TargetStoreRoot, This);
                  'POST' ->
                    do_post(Htx, TargetStoreRoot, This);
                  _ ->
            	    hty_tx:method_not_allowed(["GET", "POST"], Htx)
              end;
            false ->
               hty_tx:not_found(Htx)
        end;
    _ ->
      hty_tx:server_error("Pathmapper not found", Htx)
  end.

do_get(Htx, TargetStoreRoot, _This) ->
    Results = case hty_tx:path_below(Htx) of
        [] -> [];
        Globpath ->
            hty_glob:find(Globpath, TargetStoreRoot)
    end,
    Htx0 = hty_tx:rsp_header("Content-Type", "text/urilist", Htx),
    Pathlist = lists:map(fun(Elem) -> 
        string:join(Elem, "/")
    end, Results),
    Body = string:join(Pathlist, "\r\n"),
    Htx1 = hty_tx:echo(Body, Htx0),
    Htx2 = hty_tx:ok(Htx1),
    hty_tx:commit(Htx2).
  
do_post(Htx, TargetStoreRoot, This) ->
    FormSchema = [
		{<<"root">>, [], [{occurs, 1, 1}]},
		{<<"glob">>, [], [{occurs, 1, 1}]}
	], 
	case get_proc_store_folder(Htx, This) of
	    {ok, ProcStoreFolder} ->
            Lastidfile = get_last_id_file(Htx, ProcStoreFolder, This),
        	hty_tx:recvform(FormSchema, fun(Form, Htx2) -> 
            	{_, [_Root], _} = 
            	    lists:keyfind(<<"root">>, 1, Form),
        	    {_, [Glob], _} = 
        	        lists:keyfind(<<"glob">>, 1, Form),
        	    {ok, JobId} = hty_seqfile:incr(hty_fspath:path(Lastidfile)),
        	    JobPath = "glob_" ++ JobId,
        	    JobFolder = hty_fspath:subpath([JobPath], ProcStoreFolder),
        	    Sinkfile = hty_fspath:subpath(["result.urilist"], JobFolder),
        	    Globsegs = string:tokens(binary_to_list(Glob), "/"),
        	    Job = fun() -> 
        	        hty_fspath:mkdir(JobFolder),
        	        hty_fspath:append("", Sinkfile),
        	        hty_glob:find(Globsegs, TargetStoreRoot, fun(Result) ->
        	            hty_fspath:append(lists:join("/", Result), Sinkfile),
        	            hty_fspath:append("\r\n", Sinkfile)
        	        end),
        	        "0"
        	    end,
        	    case complete(Job, JobFolder) of        	        
        	        _Pid ->
        	            Baseurl = baseurl(Htx, This),
        	            Location = Baseurl ++ "/" ++ JobPath,
        	            {ok, hty_tx:commit(hty_tx:see_other(Location, Htx2))}
        	    end
        	end, Htx);
        {no, Why} -> hty_tx:server_error(Why, Htx)
    end.
    
get_proc_store_folder(Htx, This) ->
    ProcStoreKey = This#hty_glob_resource.proc_store_key,
    case hty_pathmapper:htx_to_fspath(Htx, ProcStoreKey, [<<"/">>]) of
	    {ok, ProcStoreRoot} ->
	        case hty_fspath:exists(ProcStoreRoot) of
	            true -> 
	                ProcStoreFolder = hty_fspath:subpath(["glob"], ProcStoreRoot),
	                case hty_fspath:mkdir(ProcStoreFolder) of
	                    ok -> {ok, ProcStoreFolder};
	                    {error, eexist} -> {ok, ProcStoreFolder};
	                    {error, Error} -> {error, Error}
	               end;
	            false -> {no, "Missing proc folder for glob " ++ hty_fspath:path(ProcStoreRoot)}
	        end;
	    _ ->
	        {no, "Proc Pathmapper not found"}
	end.

get_last_id_file(_Htx, ProcStoreFolder, _This) ->
    hty_fspath:subpath(["lastid"], ProcStoreFolder).
	
baseurl(Htx, This) ->
    case This#hty_glob_resource.location_prefix of
        {val, Val} -> Val;
        {key, Key} -> 
            case hty_tx:bound(Key, Htx) of
	            no -> "/";
	            {ok, Baseurl} -> Baseurl
	        end
	end.
	
	
complete(Job, Folder) ->
    spawn(fun() ->
        Exit = Job(),
        File = hty_fspath:subpath(["exitcode"], Folder),
        ok = hty_fspath:save(Exit, File)
    end).

