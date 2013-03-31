-module(ogrebear).
-export([start/0]).

%-behaviour(siteengine).
-include("donner.hrl").

start() ->
    mnesia:start(),
    donner:install(),
    Mimemap = [],
    Data = #donner_entry{path=["data"], entry=collection:new(Mimemap)}, 
    Tx = fun() -> mnesia:write(Data) end,
    case mnesia:transaction(Tx) of
	{aborted, Error} -> log:log("Ogrebear startup error"), log:log(Error);
	{atomic, ok} -> log:log("Ogrebear started OK")
    end.

%next(Rpath, Next) ->
    