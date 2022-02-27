-module(hty_dbi_resource).
-record(hty_dbi_resource, {key, driver, opts}).
-export([mount/2, handle/2]).

mount(Fspath, Mountext) ->
    case lists:reverse(hty_fspath:path(Fspath)) of
        ["dbi", Key | _] ->
            case file:consult(Fspath) of
                {ok, Terms} ->
                    case Terms of
                        [] -> 
                            {error, ["empty file ", Fspath]};
                        [Opts] ->  
                            #{driver := Drivermod} = Opts,
                            % Senare ...
                            % Driver = hty_pool:create(Key, Opts)
                            Driver = Drivermod:new(Key, Opts),
                            {ok, #hty_dbi_resource{key = Key, driver = Driver, opts = Opts}}
                    end;
                {error, ["failed reading ", Fspath]}
            end;
        _ -> 
            {error, ["missing key parameter"]
    end.
            
            
handle(Htx, This) ->
    hty_tx:bind(Dbikey, This).