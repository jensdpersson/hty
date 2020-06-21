-module(hty_sql_resource).
-record(this, {sqldir, dbkey, resultkey}).
-export([mount/1, handle/2]).

mount(Fspath) ->
    case hty_fspath:isdir(Fspath) of
        true ->
            case lists:reverse(hty_fspath:parts(Fspath)) of
                ["sql", Datasource, Resultkey|_] ->
                    {ok, #this{sqldir=Fspath, dbkey=Datasource, resultkey=Resultkey}};
                _ ->
                    {error, "Need sql resource needs Datasource parameter"}
                end;
        false ->
            {err, enotdir, hty_fspath:path(Fspath)}
    end.
    
handle(Htx, This) -> 
    
    % Key for looking up db in transaction context
    DbKey = This#this.dbkey,
    
    % Now get db
    case hty_tx:bound(DbKey, Htx) of
        no -> 
            hty_tx:server_error("Datasource key not bound for sql resource", Htx);
        {ok, Datasource} ->
            Meth = hty_tx:method(Htx),
            Sqlfilename = string:lowercase(atom_to_list(Meth)) ++ ".sql",
            Fspath = This#this.sqldir,
            Sqlfile = hty_fspath:subpath([Sqlfilename], Fspath),
            case hty_fspath:exists(Sqlfile) of
                false -> 
                    hty_tx:method_not_allowed([], Htx);
                true ->
                    hty_tx:server_error("not implemented", Htx)
                    %{ok, Content} = hty_fspath:load(Sqlfile),
                    %Query = hty_dbi:parse(Content, Datasource),
                    %Statement = hty_dbi:prepare(Query, Datasource),
                    %Result = hty_dbi:execute(Statement, Datasource),
                    %hty_tx:bind(This#this.resultkey, Result, Htx)
            end
    end.