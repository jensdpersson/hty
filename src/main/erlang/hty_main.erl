% @doc Entry point and central hub for the Haughty web server.
% Servers are added with listen() and web sites mounted with serve().
-module(hty_main).

-export([start/0]).
-export([reload/1, status/0]).

%External API
reload(Fscursor) ->
    Rules = [
	     hty_listen_rule,
	     hty_site_rule
	    ],
    Cfg = Fscursor:walk(Rules),
    io:format("Reloading configuration ~p~n", [Cfg]),
    Sort = fun(Item, {Ls, Ss, Is}) -> 
		   case Item of 
		       {ok, Cmd, _Path, _Rule} -> 
			   case Cmd of
			       {listen, _Proto, _Port, _Root} = L ->
				   {[L|Ls], Ss, Is};
			       {site, _Name, _Root} = S ->
				   {Ls, [S|Ss], Is}
			   end;
		       {no, _Reason, _Path, _Ruleinfo} = I ->
			   {Ls, Ss, [I|Is]}
		   end
	   end,
    A0 = {[],[],[]},
    {Listens, Sites, _Ignored} = lists:foldl(Sort, A0, Cfg),
    ?MODULE ! {reload, Listens, Sites},
	io:format("Before recv"),
    receive 
		{ok, _Status} -> ok;
		{no, _Reason} -> no
    after
		60000 -> timeout
    end.

start() ->
    Dispatcher = spawn(fun() ->
    	       loop_dispatch(hty_state:new([],[],[])) 
					   end),
    register(?MODULE, Dispatcher),
    ok.

status() ->
	 io:format("a0"),
	 ?MODULE ! {status, self()},
	 io:format("a"),
	 receive
		{status, Status} -> 
		 	 io:format("b"),
			 io:format("~p~n", [Status]);
		Other -> io:format("Bad status ~p~n",[Other])
	 after 
		10000 -> io:format("timeout")
	 end.
		
	     

loop_dispatch(ServerState) ->
    io:format("LoopDispatch"),
    receive
     	{status, ReplyTo} ->
	    	ReplyTo ! {status, ServerState}, 
	    	loop_dispatch(ServerState);
		{stop, ReplyTo} -> 
			ReplyTo ! {stopping}, 
			init:stop();
		{reload, Listens, Sites} ->
			ServerState1 = reload_servers(ServerState, Listens),
			ServerState2 = reload_sites(ServerState1, Sites),
	    	loop_dispatch(ServerState2);
		{root, ReplyTo} ->
	    	ReplyTo ! {root, hty_root:new(ServerState:sites())},
			loop_dispatch(ServerState);
		SomeMessage -> 
            io:format("Unknown message ~p~n", [SomeMessage]),
            loop_dispatch(ServerState)
    end.



reload_servers(ServerState, Listens) ->
    Servers = ServerState:servers(),
    Cmp = fun(Server, Listen) ->
		  Port = Server:port(),
		  Proto = Server:protocol(),
		  case Listen of 
		      {listen, Proto, Port} -> true;
		      _ -> false
		  end
	  end,
    Ctor = fun(Listen) -> 
		   Ip = {0,0,0,0},
		   {listen, _Proto, Port, Root} = Listen,
		   Server = hty_server:new(Ip, Port, Root),
		   Server:start(),
		   Server
	   end,
    Cull = fun(Server) -> Server:stop() end,
    Vector = hty_vector:new(Cmp, Cull, Ctor),
    Servers1 = Vector:update(Servers, Listens),
    
    ServerState:servers(Servers1).

reload_sites(ServerState, Sitespecs) -> 
    Sites = ServerState:sites(),
    Cmp = fun(Site, Spec) ->
		  Name = Site:name(),
		  case Spec of 
		      {site, Name, _Root} -> true;
		      _ -> false
		  end
	  end,
    Ctor = fun(Sitespec) -> 
		   {site, Name, Root} = Sitespec,
		   hty_site:new(Name, [Name], Root)
	   end,
    Cull = fun(_Site) -> ok end,
    Vector = hty_vector:new(Cmp, Cull, Ctor),
    Sites1 = Vector:update(Sites, Sitespecs),
    
    ServerState:sites(Sites1).
