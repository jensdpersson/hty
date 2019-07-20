-module(hty_fs).

-callback type(Path::string()) -> 'file'|'dir'|'both'|'none'.

-callback list(Path::string()) -> Subs :: [string()].

-callback last_modified(Path::string()) -> file:datetime().

-callback send(Path::string(), hty_tx:htx()) -> hty_tx:htx().

-callback recv(Path::string(),
               Spafs::[hty_spaf:spaf()],
               hty_tx:htx()) -> hty_tx:htx().

-callback exists(Path::string()) -> boolean().

-callback has_subs(Path::string()) -> boolean().

-callback mkdir(Path::string()) -> boolean().

-callback append(Path::string(), Data::binary()) -> ok | {error, Error::any()}.

-callback load(Path::string()) -> {ok, Content::binary()} | {error, Error::any()}.

-callback save(Path::string(),
               Data::binary()) -> ok | {error, Error::any()}.
