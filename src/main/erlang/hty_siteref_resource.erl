-module(hty_siteref_resource, [Siteid]).

-export([handle/1]).

handle(Htx) ->
    hty_main ! {root, Siteid, self()},
    receive
	{root, {ok, Root}} ->
	    Root:handle(Htx);
        {root, no} ->
            Htx:service_unavailable()
    after 
	1000 -> 
	    Htx:service_unavailable()
    end.


    
