
%
% @doc Entry point and central hub for the Haughty web server.
% Servers are added with listen() and web sites mounted with serve().
-module(hty_main).


-export([start/0, listen/2, serve/3, install/2]).
-export([mount/1]).

%External API


mount(Folder) ->
	Walker = hty_walker:new([
            hty_listen_rule, 
	    hty_vhost_rule
	]),
        Walker:walk(Folder, ?MODULE).


start() ->
    Dispatcher = spawn(fun() -> loop_dispatch(hty_state:new([],[],[])) end),
    register(?MODULE, Dispatcher),
    {ok, started}.


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
	{request, ReplyTo} ->
	    ReplyTo ! {route, hty_root:new(ServerState:sites())};
	SomeMessage -> 
            io:format("Unknown message ~p~n", [SomeMessage]),
            loop_dispatch(ServerState)
    end.








