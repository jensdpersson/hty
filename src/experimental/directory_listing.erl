-module(directory_listing).
-behaviour(resource).
-export([handle/2]).

-include("donner.hrl").
-include("helm.hrl").
-include("http.hrl").

xml(Entries) ->
    Children = lists:map(fun({_, [Path|_], _}) ->
				 #elm{name="file", at=[#at{name="name",val=pack_path([Path])}]}
			 end, Entries),
    Doc = #doc{
      root=#elm{name="dir", body=Children},
      pi=#pi{name="xml-stylesheet", data="type=\"text/xsl\" href=\"/dir.xsl\""}
     },
    Payload = helm:pack(Doc),
    {"text/xml", Payload}.

urilist(Entries) ->
    Payload = lists:map(fun({_, [Path|_], _}) ->
                            [pack_path([Path]), "\n"]
			 end, Entries),
    {"text/uri-list", Payload}.

handle(Http, #donner_evt{db=Id, above=Above}) ->
    Entries = donner:list(Id, Above),
    Accept = case lists:keysearch('Accept', 1, Http#http.headers) of
        {value, {_, A}} -> A;
        _ -> none
    end,
    {ContentType, Payload} = case Accept of
        "text/uri-list" -> urilist(Entries);
        _ -> xml(Entries)
    end,
    #http{status={200, "OK"},
	  headers=[{"Content-Type", ContentType}],
	  entity=Payload
	 }.

pack_path(Xs) -> pack_path(Xs, []).
pack_path([X], []) -> [X];
pack_path([X|Xs], []) -> pack_path(Xs, [$/,X]);
pack_path([X], Ys) -> pack_path([], [X|Ys]);
pack_path([X|Xs], Ys) -> pack_path(Xs, [$/,X|Ys]);
pack_path([], Ys) -> Ys.
