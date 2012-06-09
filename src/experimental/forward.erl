-module(forward, [ParamName, ParamValue, Target]).
-behaviour(resource).
-export([handle/2]).

-include("http.hrl").
-include("donner.hrl").

handle(R, Evt) ->
    #http{path=Path} = R, 
    #donner_evt{above=[This|_], params=Params} = Evt,
    io:format("Fwd ~p -> ~p with ~p=~p~n",[Path,Target,ParamName,This]),
    Evt1 = Evt#donner_evt{above=Target, params=[{ParamName,replace([])}|Params]},
    case donner:next_in_path(Evt1) of
	{ok, {_Db,_Path,Resource}} -> Resource:handle(R, Evt1);
	{no, Path} -> #http{status="404 NOT FOUND"} 
    end.

replace(_Vars) -> ParamValue.



    
	    
