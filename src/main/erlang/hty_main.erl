% @doc Entry point and central hub for the Haughty web server.
-module(hty_main).

-export([main/1]).
-export([start/0]).
-export([reload/1, status/0]).

loop_cli() ->
    receive
	     stop -> ok
    after 10000 ->
	    loop_cli()
    end.

print(M) -> io:format(M), io:format("~n").

main(Argv) ->
  case hty_ctl:start(Argv) of
	   {ok,started} ->
        print(os:getpid()),
	      loop_cli();
	   {no, Error} ->
	      print(Error);
	   {error, Error} ->
	      print(Error)
  end.


reload(Fspath) ->
  Rules = [
     hty_listen_rule,
     hty_site_rule
    ],
  Transforms = [],
  Walker = hty_walker:new(Fspath, Rules, Transforms),
  Cfg = Walker:walk(),
  reconfigure(Cfg).


mount(Fspath) ->
  Parent = hty_root_directive:new(),
  reconfigure(hty_mounter:mount(Parent, Fspath)).

reconfigure(Cfg) ->

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
    ?MODULE ! {reload, Listens, Sites, self()},
    io:format("Before recv"),
    receive
	{ok, Status} -> {ok, Status};
	{no, Reason} -> {error, Reason}
    after
	60000 -> timeout
    end.

start() ->
    spawn(fun loop_supervise/0),
    ok.

loop_supervise() ->
    process_flag(trap_exit, true),
    Dispatcher = spawn_link(fun() ->
                    loop_dispatch(hty_state:new([],[]))
                 end),
    register(?MODULE, Dispatcher),
    receive
        {'EXIT', _FromPid, Reason} ->
	    case Reason of
		normal ->
		    io:format("Normal EXIT trapped, stopping~n");
		_ ->
		    io:format("Restarting due to ~p ~n", [Reason]),
		    loop_supervise()
	    end
    end,
    ok.

status() ->
    ?MODULE ! {status, self()},
    receive
	{status, Status} ->
	    Status;
	Other ->
	    io:format("Bad status ~p~n",[Other])
    after
	10000 -> io:format("timeout")
    end.



loop_dispatch(ServerState) ->
    %io:format("LoopDispatch~n"),
    receive
     	{status, ReplyTo} ->
	    	ReplyTo ! {status, ServerState},
	    	loop_dispatch(ServerState);
	{stop, ReplyTo} ->
		ReplyTo ! stopping;
	%	init:stop();
	{reload, Listens, Sites, ReplyTo} ->
		ServerState1 = reload_servers(ServerState, Listens),
		ServerState2 = reload_sites(ServerState1, Sites),
                ReplyTo ! {ok, started},
	    	loop_dispatch(ServerState2);
	{root, SiteID, ReplyTo} ->
	    	ReplyTo ! {root, hty_util:find(fun(S) ->
                                             S:match(SiteID)
                                           end, ServerState:sites())},
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
		   {listen, Proto, Port, Root} = Listen,
		   Server = hty_server:new(Proto, Ip, Port, Root),
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
