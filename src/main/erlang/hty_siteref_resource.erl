-module(hty_siteref_resource).

-export([handle/2, new/1]).

-record(hty_siteref_resource, {siteid}).

new(Siteid) ->
    #hty_siteref_resource{siteid=Siteid}.

handle(Htx, This) ->
    Siteid = This#hty_siteref_resource.siteid,
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


    
