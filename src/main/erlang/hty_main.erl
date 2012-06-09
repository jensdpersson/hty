
%
% @doc Entry point and central hub for the Haughty web server.
% Servers are added with listen() and web sites mounted with serve().
-module(hty_main).


-export([start/0, listen/2, serve/3, install/2]).
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
		       	    {listen, _Proto, _Port} = L ->
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
	receive 
		{ok, _Status} -> ok;
		{no, _Reason} -> no
	after
		60000 -> timeout
	end.

start() ->
    Dispatcher = spawn(fun() ->
    	       loop_dispatch(hty_state:new([],[],[])) end),
    register(?MODULE, Dispatcher),
    ok.


%@doc Ip is an ipv4 address as a 4-tuple of bytes, Port is an integer
listen(Ip, Port) -> 
    ?MODULE ! {listen, Ip, Port, self()}, 
    receive
	{ok, _} -> ok;
	{error, Error} -> Error
    after 
	5000 -> 
	    ?MODULE ! {no, {listen, Ip, Port, self()}},
	    {timeout, 5000}
    end.

%@doc mount the folder DocRoot using EngineId and call it SiteId 
serve(SiteId, EngineId, DocRoot) -> 
    ?MODULE ! {mount, SiteId, EngineId, DocRoot, self()}, 
    receive
	{ok, _} -> ok;
	{error, Error} -> Error
    after 
	5000 -> 
	    ?MODULE ! {no, {mount, SiteId, EngineId, DocRoot, self()}},
	    {timeout, 5000}
    end.


%@doc install a new site engine, call it Id
install(EngineId, Engine) -> 
    ?MODULE ! {install, EngineId, Engine},
    receive
	{ok, _} -> ok;
	{error, Error} -> Error
    after 
	5000 -> 
	    ?MODULE ! {no, {install, EngineId, Engine}},
	    {timeout, 5000}
    end.

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
        {mount, SiteId, EngineId, DocRoot, ReplyTo} -> 
	    spawn(fun() -> 
			  case ServerState:get_engine(EngineId) of
			      {ok, Engine} ->
				  case Engine:site(DocRoot) of
				      {ok, Site} -> ?MODULE ! {prepared, SiteId, Site, ReplyTo};
				      Error -> ReplyTo ! {error, Error}
				  end;
			      no ->
				  ReplyTo ! {error, {site_engine_not_loaded, EngineId}}
			  end
		  end),
	    loop_dispatch(ServerState);
	{prepared, Site, ReplyTo} ->
	    ReplyTo ! {ok, mounted},
	    loop_dispatch(ServerState:add_site(Site));
	{status, ReplyTo} ->
	    ReplyTo ! {status, ServerState}, 
	    loop_dispatch(ServerState);
	{stop, ReplyTo} -> ReplyTo ! {stopping}, init:stop();
	{load_engine, EngineId, EngineModule, ReplyTo} ->
	    spawn(fun() -> 
			  case EngineModule:start() of
			      {ok, Engine} -> 
				  ?MODULE ! {engine_loaded, EngineId, Engine};
			      {error, Error} -> 
				  ReplyTo ! {error, Error}
			  end
		  end),
	    loop_dispatch(ServerState);
	{engine_loaded, EngineId, Engine, ReplyTo} ->
	    ReplyTo ! {ok, loaded},
	    loop_dispatch(ServerState:add_engine(EngineId, Engine));
        {listen, [Ip, Port], ReplyTo} ->
            Server = hty_server:new(Ip, Port, self()),
            case Server:start() of
                {ok, Server1} -> 
		    ReplyTo ! {ok, listening, Ip, Port},
		    loop_dispatch(ServerState:add_server(Server1));
                {error, Error} -> 
		    ReplyTo ! {error, Error},
		    loop_dispatch(ServerState)
	    end;
	{reload, Listens, Sites} -> 
		 Servers = ServerState:servers(),
		 Cmp = fun(Server, Listen) ->
		    Port = Server:port(),
		    Proto = Server:protocol(),
		    case Listen of 
		    	 {listen, Proto, Port} -> true;
			 _ -> false
		    end,
		 Ctor = fun(Listen) -> 
		      Ip = {0,0,0,0},
		      {listen, Proto, Port} = Listen,
		      Server = hty_server:new(Ip, Port),
		      Server:start(),
		      Server
		 end,
		 Cull = fun(Server) -> Server:stop() end,
		 Vector = hty_vector:new(Cmp, Cull, Ctor),
		 Servers1 = Vector:filter(Servers, Listens),

		 State1 = ServerState:servers(Servers1),

		 

		 loop_dispatch();
	{request, ReplyTo} ->
	    ReplyTo ! {route, hty_root:new(ServerState:sites())};
	SomeMessage -> 
            io:format("Unknown message ~p~n", [SomeMessage]),
            loop_dispatch(ServerState)
    end.








