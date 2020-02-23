-module(hty_main).

-export([main/1]).
-export([start/0, stop/0]).
-export([status/0]).
-export([start/1]).

main([]) ->
  io:format("Usage: hty <path-to-site>~n");
main(_) ->
  case start() of
    {error, Error} ->
      io:format("hty start status: fail ~p~n", [Error]),
      {error, Error};
    {ok, Status} ->
      io:format("hty start status: started ~p~n", [Status]),
      receive
        stop -> ok
      end
  end.

mount(Path0) ->
  Path = filename:absname(Path0),
  io:format("Mounting ~p~n", [Path]),
  Fspath = hty_fspath:new(Path),
  case hty_fspath:exists(Fspath) of
    true ->
      case hty_mounter:walk(Fspath, "server") of
        {error, Error} ->
          {error, Error};
        {ok, Servers} ->
          ?MODULE ! {reload, Servers, self()},
          io:format("Reloading cfg ~p~n", [Servers]),
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
      io:format("Path does not exist from where I stand (~p)~n", [file:get_cwd()]),
      {error, {enoent, Path}}
  end.

start() ->
  Args = init:get_plain_arguments(),
  erlang:display(Args),
  [Path|_] = case Args of
    % For running in shell
    [_, "shell"|Realargs] -> Realargs;
    ["console"|Realargs] -> Realargs;
    % For running as an escript
    [Head|Realargs] ->
        case lists:reverse(Head) of 
            [$e, $v, $r, $e, $s, $y, $t, $h|_] -> Realargs;
            _ -> Args
        end;
    _ -> Args
  end,
  start(Path).
  
start(Path) ->
  case spawn(fun loop_supervise/0) of
    Pidko when is_pid(Pidko) ->
      case mount(Path) of
        {ok, started} ->
          {ok, Pidko};
        Error -> Error
      end;
    Other ->
      {error, Other}
  end.

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
  io:format("hty supervisor starting up~n"),
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
  io:format("hty supervisor closing down~n"),
  ok.

loop_dispatch(Servers) ->
  receive
    {status, ReplyTo} ->
      ReplyTo ! {status, Servers},
        loop_dispatch(Servers);
    {stop, ReplyTo} ->
      Result = (catch lists:foreach(fun(Server) -> hty_server:stop(Server) end, Servers)),
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
      {ok, New1} = hty_server:start(New),
      New1
    end,
    Cull = fun(Server) ->
      hty_server:stop(Server)
    end,
    Vector = hty_vector:new(Cmp, Cull, Ctor),
    hty_vector:update(Olds, News, Vector).

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
