-module(bind, [Var]).
-export([handle/2]).

-include("http.hrl").
-include("donner.hrl").

handle(R, Evt) ->
    #http{path=Path} = R,
    #donner_evt{above=[This|_], params=Params} = Evt,
    Evt1 = Evt#donner_evt{above=fix_this, params=[{Var, This}|Params]},
    case donner:next_in_path(Evt1) of
	{ok, {_Db,_Path,Resource}} -> Resource:handle(R, Evt1);
	{no, Path} -> #http{status="404 NOT FOUND"}
    end.

