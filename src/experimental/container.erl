-module(container).
-behaviour(resource).
-export([handle/2]).

-include("donner.hrl").
-include("http.hrl").

handle(#http{method=Method}, #donner_evt{db=_Id, above=_Above}) ->
    case Method of
	'POST' -> 405;
	'GET' -> 405;
	_ -> 405
    end.


