-module(hty_neatface_resource).
-record(hty_neatface_resource, {pid}).
-export([mount/2, handle/2]).

mount(Fspath, Mountctx) ->
    case hty_mounter:walk(Fspath, "neatface", Mountctx)  of
	{error, Error} ->
	    {error, Error};
	{ok, [Neatface]} ->
	    {ok, Neatfacepid} = hty_neatface:start_link(Neatface),
	    {ok, #hty_neatface_resource{pid=Neatfacepid}};
	{ok, _} -> 
	    {error, "More than one neatface is not supported"}
    end.

handle(Htx, This) ->
    Neatpath = extract_path(Htx),
    Pid = This#hty_neat_resource.pid,
    ReplyTo = self(),
    case hty_tx:method(Htx) of
	'POST' ->
	    SomeKindOfDataStructure = extract_payload(Htx),
	    Pid ! {append, Neatpath, SomeKindOfDataStructure, ReplyTo},
	    recv();
	'GET' ->
	    Pid ! {query, Neatpath, ReplyTo},
	    recv();
	_ -> 
	    hty_tx:method_not_allowed(['GET', 'POST'], Htx)
    end.

recv() ->
    receive 
	{SomeKindOfResponse} -> no
    end.
    
extract_path(Htx) ->
    Pathbelow = hty_tx:path_below(Htx),
    Segments = lists:map(fun(Urisegment) -> 
				 
			 end, Pathbelow),
    
    
    
