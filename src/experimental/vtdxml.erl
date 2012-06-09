-module(vtdxml, [ContentType, FSPath]).
-behaviour(resource).
-export([handle/2]).

-include("http.hrl").
-include("donner.hrl").

handle(Http#http{method=Method}, Evt#donner_evt{below=Below}) ->
    case Below of
         [] ->
             %This means we act on the entire document
             case Method of
                'GET' ->
                        %deliver document
                    %check etag
                    %respond
                'PUT' -> #http{status="404 Not Found"}
            end;
         _ ->
             %Whereas we here select a part of it to act on.
             getAsBranch(Http, Evt)
    end
    


select(DataFile, Path) ->
    Steps = lists:map(Path, fun(Step) -> upath:step(Step) end),
    vixen:select(DataFile, Steps).

update(Path, Data) ->
    Steps = lists:map(Path, fun(Step) -> upath:step(Step) end),
    vixen:update(DataFile, Steps, Data).

getAsLeaf(Http, Evt) ->
    case lists:keysearch(xslpi, 1, Evt#donner_evt.params) of
	{value, {xslpi, Xslpi}} ->
	    respond([<<"<?xml-stylesheet type=\"text/xsl\" href=\"">>, Xslpi, <<"\" ?>">>, Data]);
	 _ -> respond(Data)
    end.

respond(Entity) ->
    #http{status="200 OK", headers=[{'Content-Type', ContentType}], entity=Entity}.



    
	    
