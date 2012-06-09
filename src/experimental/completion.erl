-module(completion, [Datatree, Mod]).
-export([handle/2]).
-behaviour(resource).

-include("http.hrl").
-include("donner.hrl").

handle(Http, Ctx) ->
    case Http#http.method of
        'GET' -> do_GET(Http, Ctx);
        'POST' -> do_POST(Http, Ctx);
        'PUT' -> do_PUT(Http, Ctx);
        'DELETE' -> do_DELETE(Http, Ctx)
    end.


do_GET(#http{query_parms=Parms}, #donner_evt{below=Below, db=Db}) ->
    Max = case lists:keysearch(max, 1, Parms) of
              {value, Max0} -> Max0;
              false -> -1
    end,
    case Below of
        [] ->
            traverse(Datatree, Max, [], Db);
        Rs ->
            traverse(Datatree ++ Rs, Max, [], Db)

    end.

traverse(_, 0, Acc, _) -> Acc;
traverse(Path, Left, Acc, Db) ->
    case donner:lookup(Db, Path) of
        not_found -> Acc;
        {_,_,R} ->
            Head = #http{method='HEAD'},
            Ctx = #donner_evt{below=Path},
            [_|Ps] = Path,
            case R:handle(Head, Ctx) of
                #http{status="200 OK", entity=Entity} ->
                    traverse(Ps, Left-1, [Entity|Acc], Db);
                _ ->
                    traverse(Ps, Left, Acc, Db)
            end
    end.


%for now...
do_POST(Http, Ctx) -> Http, Ctx, #http{status="405 Method not allowed"}.
do_PUT(Http, Ctx) -> Http, Ctx, #http{status="405 Method not allowed"}.
do_DELETE(Http, Ctx) -> Http, Ctx, #http{status="405 Method not allowed"}.