-module(hty_dbi_resource).
-record(hty_dbi_resource, {key, dbi}).
-export([mount/2, handle/2]).

mount(Fspath, _Mountext) ->
    case lists:reverse(hty_fspath:path(Fspath)) of
        ["dbi", Key | _] ->
            case file:consult(Fspath) of
                {ok, Terms} ->
                    case Terms of
                        [] -> 
                            {error, ["empty file ", Fspath]};
                        [ConnSpec] ->  
                            case hty_dbi:connect(ConnSpec) of
                                {ok, Dbi} ->
                                    {ok, #hty_dbi_resource{key = Key, dbi = Dbi}};
                                {error, Error} ->
                                    {error, Error}
                            end
                    end;
		        _ ->
		            {error, ["failed reading ", Fspath]}
            end;
        _ -> 
            {error, ["missing key parameter"]}
    end.
            
            
handle(Htx, This) ->
    Key = This#hty_dbi_resource.key,
    Dbi = This#hty_dbi_resource.dbi,
    hty_tx:bind(Key, Dbi, Htx).
