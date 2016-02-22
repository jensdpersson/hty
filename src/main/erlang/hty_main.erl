% @doc Entry point and central hub for the Haughty web server.
-module(hty_main).

-export([main/1]).
-export([start/0, stop/0]).
-export([status/0]).

%loop_cli() ->
%    receive
%     stop -> ok
%    after 10000 ->
%	    loop_cli()
%    end.

%print(M) -> io:format(M), io:format("~n").

main([_, Path]) ->
  case start() of
    ok -> mount(Path);
    Other -> {error, Other}
  end.

%reload(Fspath) ->
%  Rules = [
%     hty_listen_rule,
%     hty_site_rule
%    ],
%  Transforms = [],
%  Walker = hty_walker:new(Fspath, Rules, Transforms),
%  Cfg = Walker:walk(),
%  reconfigure(Cfg).

mount(Path) ->
  Fspath = hty_fspath:new(Path),
  case Fspath:exists() of
    true ->
      case hty_mounter:walk(Fspath, "server") of
        {error, Error} ->
          {error, Error};
        {ok, Servers} ->
          ?MODULE ! {reload, Servers, self()},
          io:format("Reloading cfg"),
          receive
            {ok, Status} ->
              {ok, Status};
            {error, Reason} ->
              {error, Reason}
          after
            60000 -> timeout
          end
      end;
    false ->
      {error, enoent, Path}
  end.

%reconfigure(Cfg) ->
%    io:format("Reloading configuration ~p~n", [Cfg]),
%    Sort = fun(Item, {Ls, Ss, Is}) ->
%		   case Item of
%		       {ok, Cmd, _Path, _Rule} ->
%			   case Cmd of
%			       {listen, _Proto, _Port, _Root} = L ->
%				   {[L|Ls], Ss, Is};
%			       {site, _Name, _Root} = S ->
%				   {Ls, [S|Ss], Is}
%			   end;
%		       {no, _Reason, _Path, _Ruleinfo} = I ->
%			   {Ls, Ss, [I|Is]}
%		   end
%	   end,
%    A0 = {[],[],[]},
%    {Listens, Sites, _Ignored} = lists:foldl(Sort, A0, Cfg),
%    ?MODULE ! {reload, Listens, Sites, self()},
%    io:format("Before recv"),
%    receive
%	{ok, Status} -> {ok, Status};
%	{no, Reason} -> {error, Reason}
%    after
%	60000 -> timeout
%    end.

start() ->
  spawn(fun loop_supervise/0),
  ok.

stop() ->
  ?MODULE ! {stop, self()},
  receive
    stopping -> ok;
    {error, _} = Error ->
      Error
  after
    60000 -> timeout
  end.

status() ->
  ?MODULE ! {status, self()},
  receive
    {status, Status} ->
      Status;
    Other ->
      {error, {"Bad status", Other}}
  after
    10000 -> io:format("timeout")
  end.

loop_supervise() ->
  process_flag(trap_exit, true),
  Dispatcher = spawn_link(fun() ->
    loop_dispatch([])
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

loop_dispatch(Servers) ->
  receive
    {status, ReplyTo} ->
      ReplyTo ! {status, Servers},
        loop_dispatch(Servers);
    {stop, ReplyTo} ->
      Result = (catch lists:foreach(fun(Server) -> Server:stop() end, Servers)),
      case Result of
        ok ->
          ReplyTo ! stopping;
        Error ->
          ReplyTo ! {error, Error}
      end;
    {reload, NewServers, ReplyTo} ->
      Result = (catch reload_servers(Servers, NewServers)),
      case Result of
        Servers2 when is_list(Servers2) ->
          ReplyTo ! {ok, started},
          loop_dispatch(Servers2);
        Error ->
          ReplyTo ! {error, Error},
          loop_dispatch(Servers)
      end;
    SomeMessage ->
      io:format("Unknown message ~p~n", [SomeMessage]),
      loop_dispatch(Servers)
  end.

%loop_dispatch_old(ServerState) ->
%  receive
%    {status, ReplyTo} ->
%      ReplyTo ! {status, ServerState},
%        loop_dispatch(ServerState);
%    {stop, ReplyTo} ->
%      ReplyTo ! stopping;
%    {reload, Listens, Sites, ReplyTo} ->
%      ServerState1 = reload_servers(ServerState, Listens),
%      ServerState2 = reload_sites(ServerState1, Sites),
%      ReplyTo ! {ok, started},
%      loop_dispatch(ServerState2);
%    {root, SiteID, ReplyTo} ->
%      ReplyTo ! {root, hty_util:find(fun(S) ->
%        S:match(SiteID)
%      end, ServerState:sites())},
%      loop_dispatch(ServerState);
%    SomeMessage ->
%      io:format("Unknown message ~p~n", [SomeMessage]),
%      loop_dispatch(ServerState)
%    end.

reload_servers(Olds, News) ->
    Cmp = fun(Old, New) ->
      Old:signature() =:= New:signature()
    end,
    Ctor = fun(New) ->
      {ok, New1} = New:start(),
      New1
    end,
    Cull = fun(Server) ->
      Server:stop()
    end,
    Vector = hty_vector:new(Cmp, Cull, Ctor),
    Vector:update(Olds, News).

%reload_servers_old(ServerState, Listens) ->
%    Servers = ServerState:servers(),
%    Cmp = fun(Server, Listen) ->
%		  Port = Server:port(),
%		  Proto = Server:protocol(),
%		  case Listen of
%		      {listen, Proto, Port} -> true;
%		      _ -> false
%		  end
%	  end,
%    Ctor = fun(Listen) ->
%		   Ip = {0,0,0,0},
%		   {listen, Proto, Port, Root} = Listen,
%		   Server = hty_server:new(Proto, Ip, Port, Root),
%		   Server:start(),
%		   Server
%	   end,
%    Cull = fun(Server) -> Server:stop() end,
%    Vector = hty_vector:new(Cmp, Cull, Ctor),
%    Servers1 = Vector:update(Servers, Listens),
%
%    ServerState:servers(Servers1).

%reload_sites(ServerState, Sitespecs) ->
%    Sites = ServerState:sites(),
%    Cmp = fun(Site, Spec) ->
%		  Name = Site:name(),
%		  case Spec of
%		      {site, Name, _Root} -> true;
%		      _ -> false
%		  end
%	  end,
%    Ctor = fun(Sitespec) ->
%		   {site, Name, Root} = Sitespec,
%		   hty_site:new(Name, [Name], Root)
%	   end,
%    Cull = fun(_Site) -> ok end,
%    Vector = hty_vector:new(Cmp, Cull, Ctor),
%    Sites1 = Vector:update(Sites, Sitespecs),
%
%    ServerState:sites(Sites1).
