-module(document, [ContentType, Data]).
-behaviour(resource).
-export([handle/2]).

-include("http.hrl").
-include("donner.hrl").

handle(_, Evt) ->
    case lists:keysearch(xslpi, 1, Evt#donner_evt.params) of
	{value, {xslpi, Xslpi}} ->
	    respond([<<"<?xml-stylesheet type=\"text/xsl\" href=\"">>, Xslpi, <<"\" ?>">>, Data]);
	_ -> respond(Data)
    end.

respond(Entity) ->
    #http{status="200 OK", headers=[{'Content-Type', ContentType}], entity=Entity}.



    
	    
