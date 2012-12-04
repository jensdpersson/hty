-module(hty_siteref_resource, [Siteid]).

-export([handle/1]).

handle(Htx) ->
	hty_main ! {root, self()},
	receive
		{root, Root} ->
			Root:handle(Htx)
	after 
		1000 -> 
			Htx:service_unavailable()
    end.


    
