-module(collection, [Mimemap, IndexResource]).
-behaviour(resource).
-export([handle/2]).

-include("donner.hrl").
-include("http.hrl").
-include("helm.hrl").

handle(Http, Evt) ->
    case Http#http.method of
	'MKCOL' -> do_mkcol(Http, Evt);
	'PUT' -> do_put(Http, Evt);
	'GET' -> do_get(Http, Evt);
	_ -> not_supported
    end.

%Don't allow mkcol on a resource that is already a collection
do_mkcol(_, #donner_evt{below=[]}) -> {error, 409};

%If there is just one more segment, then if there is a resource there
%we need to signal error. We are not allowed to remap this to a collection.
%If not we can put a new collection there.
do_mkcol(_Http, #donner_evt{below=[_], db=Id} = Evt) -> 
    case donner:next_in_path(Evt) of
	{no, Path} -> 
	    donner:store(Id, Path, collection:new(Mimemap));
	{ok, _Entry} -> {error, 405}
    end;
%For a longer subpath we just delegate if the resources exist.
do_mkcol(Http, #donner_evt{below=[Next|Nexts], above=Above} = Evt) -> 
    case donner:next_in_path(Evt) of
	{no, _} -> {error, 409};
	{ok, Entry} ->
	    Module = Entry#donner_entry.entry,
	    Module:handle(Http, Evt#donner_evt{below=Nexts, above=[Next|Above]})
    end.

mimemap(Method, Mimetype) ->
    case lists:keysearch({Method,Mimetype}, 1, Mimemap) of 
	[R] -> R;
	false -> binary_file
    end.

%
% Put will work on a direct subresource by lookup in mimetable
% Put on this resource fails.
do_put(Http, #donner_evt{below=Below, params=Params} = Evt) ->
    case Below of
	[] -> {error, 405}; % cannot remap this resource to something else
	[_] ->
	    case donner:next_in_path(Evt) of
		{no, _} -> log:log(Evt),
		      {value, {http, Http}} = lists:keysearch(http, 1, Params),
		      Mimetype = lists:keysearch("Content-Type", 1, Http#http.headers),
		      Module = mimemap(put, Mimetype),
		      Entry = Module:new(Http#http.entity),
		      Path = donner:next_in_path(Evt),
		      case mnesia:write(#donner_entry{path=Path, entry=Entry}) of
			  ok -> ok;
			  Error -> log:log(Error), {error, 500} 
		      end;
		{ok, Entry} -> 
		    Module = Entry#donner_entry.entry,
		    Module:handle(Http, Evt#donner_evt{below=[]})
	    end;
	[Next|Nexts] ->
	    case donner:next_in_path(Next, Evt#donner_evt.above) of
		{no, _} -> {error, 404};
		{ok, Entry} -> 
		    Module = Entry#donner_entry.entry,
		    Module:handle(Http, Evt#donner_evt{below=Nexts})
	    end
    end.
	    
do_get(Http, #donner_evt{below=Below, above=Above} = Evt) ->
    case Below of
	[] ->
	    case lists:reverse(Http#http.path) of 
		[$/|_] -> IndexResource:handle(Http, Evt);
		_ -> #http{status="307 SEE OTHER", headers=[{'Location', Http#http.path ++ "/"}]}
	    end;
	[Next|Nexts] ->  %this clause handles the [Next] case as well, with an empty Nexts.
	    case donner:next_in_path(Evt) of
		{ok, {_, _, Module}} -> 
		    Module:handle(Http, Evt#donner_evt{below=Nexts,above=[Next|Above]});
		{no, _Path} -> 
		    #http{status="404 NOT FOUND"}
	    end;
        Other -> io:format("Other[~p]~n",[Other])
    end.
	    


    
	    
