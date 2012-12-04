-module(hty_root, [Sites]).

-export([handle/1]).

handle(Htx) ->
    Host = Htx:request_header(host),
    case lists:keysearch(Host, 1, Sites) of
		{value, {_,Site}} ->
	    	Site:handle(Htx);
		false ->
	    	Htx:not_found()
    end.
			
    
    
