-module(hty_sql_resource).
-record(hty_sql_resource, {sqldir, dbkey, resultkey}).
-export([mount/1, handle/2]).

mount(Fspath) ->
    case hty_fspath:isdir(Fspath) of
        true ->
            case lists:reverse(hty_fspath:parts(Fspath)) of
                ["sql", Dbikey, Reskey|_] ->
                    {ok, #hty_sql_resource{sqldir=Fspath, dbkey=Dbikey, resultkey=Reskey}};
                _ ->
                    {error, "sql resource needs Datasource parameter"}
                end;
        false ->
            {err, enotdir, hty_fspath:path(Fspath)}
    end.

handle(Htx, This) ->

    % Key for looking up db in transaction context
    DbKey = This#hty_sql_resource.dbkey,

    % Now get db
    case hty_tx:bound(DbKey, Htx) of
        no ->
            hty_tx:server_error("Datasource key not bound for sql resource", Htx);
        {ok, Dbi} ->
            Meth = hty_tx:method(Htx),
            Sqlfilename = string:lowercase(atom_to_list(Meth)) ++ ".sql",
            Fspath = This#hty_sql_resource.sqldir,
            Sqlfile = hty_fspath:subpath([Sqlfilename], Fspath),
            case hty_fspath:exists(Sqlfile) of
                false ->
                    hty_tx:method_not_allowed([], Htx);
                true ->
                    hty_tx:server_error("not implemented", Htx),
                    {ok, Content} = hty_fspath:load(Sqlfile),
                    Result = hty_dbi:query(Content, Dbi),
                    hty_tx:bind(This#hty_sql_resource.resultkey, Result, Htx)
            end
    end.
