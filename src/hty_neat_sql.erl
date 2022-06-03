-module(hty_neat_sql).
-record(hty_neat_sql, {db, taxonomy}).
-export([append/3, query/2]).


append(Neatpath, Data, This) -> no.

-type segment() :: [{
    Name :: string(), 
    Preds :: [{
        Functor :: string(),  
        Params :: [string()]
    }]
}]. 

query(Neatpath, This) -> 
    Taxonomy = This#hty_neat_sql.taxonomy,
    Segments1 = lists:map(fun(Segment) ->
        {Segment, lookup_taxon(Segment, Taxonomy)}
    end, Neatpath),
    

-type taxon() :: {
        Name :: string(), 
        Key :: integer() | , 
        Columns :: [{
            Name :: string(), 
            Type :: text | int
        }]
    }.

%
% private parts
%
add_taxon(Segment, Taxonomy) -> 
    case lists:keyfind(Segment, 1, Taxonomy),
        false -> unknown;
        Taxon -> Taxon
    end.
    
% SELECT skrivs en gång, baserat på sista segmentets params

% Varje segment blir en JOIN mellan 
% JOIN attribut_tabell_enl_taxon AS uppstegat_alias 
% ON uppstegat_alias.parent_id = parent_tabell_enl_taxon.id
% AND ... predikatens villkor, inklusive pos(3) 

% men... ska en taxon innehålla parentens tabell? Nej. Steget innan innehåller
% parentens taxons tabell!


join(LastAliasNum, Segment, Taxon) ->
    AliasNum = LastAliasNum + 1,
    Sql = "JOIN " ++ Taxon#taxon.table ++ " AS a" ++ AliasNum ++
    " ON a" ++ AliasNum ++ ".parent_id=a" ++ LastAliasNum ++ ".id AND a" ++ 
    ++ AliasNum ++ ".key=" ++ Taxon#taxon.key,
    lists:foldl(fun(Pred, Sql1) ->
        Sql1 ++ pred_to_sql(Pred)
    end, Sql, Segment#neat.predicates).
    
pred_to_sql({pos, Pos}) ->
    "AND a" ++ 
    

