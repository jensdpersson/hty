-module(hty_sql).
%-record(hty_sql, {from, joins=[], wheres=[]}).

%from(Table) -> #hty_sql{from=Table}.

%-spec join(Table::atom(), Query::#hty_sql{}) -> #hty_sql{}.
%join(Table, Query) -> Query#hty_sql{joins=[{left, Table}]}.

%-spec where(Table::atom(), Query::#hty_sql{}) -> #hty_sql{}.
%join(Table, Query) -> Query#hty_sql{joins=[{left, Table}]}.

% Så, man behöver ju kunna uttrycka predikat för where och on
% ett record för det? Ett record för en join? Eller kör join samma
% record som frågan i sig?
