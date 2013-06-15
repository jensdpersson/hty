-module(hty_root, [Sites]).

-export([handle/1]).

handle(Htx) ->
    [Host] = Htx:req_header('Host'),
    Host1 = binary_to_list(Host),
    io:format("~p in? ~p~n",[Host1,Sites]),
    case hty_util:find(fun(Site) -> 
			       Site:match(Host1)
		       end, Sites) of
	{ok, Resource} ->
	    Resource:handle(Htx);
	no ->
	    Htx:not_found()
    end.
			
    
    
