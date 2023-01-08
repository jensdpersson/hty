-module(hty_dbi).
-export([connect/1, query/3]).

connect(Connspec) -> 
    Driver = element(1, Connspec),
    Driver:connect(Connspec).

query(Sql, Parameters, Dbi) -> 
    Dbi:query(Sql, Parameters).