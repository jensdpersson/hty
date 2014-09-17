-module(hty_root).
-record(hty_root, {sites}).

-export([handle/2, new/1]).

new(Sites) ->
    #hty_root{sites=Sites}.

handle(Htx, This) ->
    [Host] = Htx:req_header('Host'),
    Host1 = binary_to_list(Host),
    Sites = This#hty_root.sites,
    io:format("~p in? ~p~n",[Host1,Sites]),
    case hty_util:find(fun(Site) -> 
			       Site:match(Host1)
		       end, Sites) of
	{ok, Resource} ->
	    Resource:handle(Htx);
	no ->
	    Htx:not_found()
    end.
			
    
    
