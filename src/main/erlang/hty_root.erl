-module(hty_root, [Sites]).

-export([handle/1]).

handle(HtyRequest) ->
    Host = HtyRequest:request_header(host),
	case lists:keysearch(Host, 1, Sites) of
		{value, {_,Site}} ->
			Site:handle(HtyRequest);
		false ->
			HtyRequest:not_found()
	end.
			
    
    
