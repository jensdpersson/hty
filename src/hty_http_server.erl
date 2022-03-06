-module(hty_http_server).
%-behaviour(hty_server).
-export([mount/2]).
-export([new/4, start/1, stop/1]).
-export([protocol/1, address/1, port/1, root/1]).

-record(hty_http_server, {proto, ip, port, resource}).

new(Proto, Ip, Port, Resource) ->
    #hty_http_server{proto=Proto, ip=Ip,port=Port,resource=Resource}.


mount(Fspath, Mc) ->
  {Ip, Port} = case hty_fspath:parts(Fspath) of
    [PortSpec, "http"] ->
      {{0,0,0,0}, list_to_integer(PortSpec)};
    [A,B,C,D,PortSpec, "http"] ->
      {{list_to_integer(A),list_to_integer(B),list_to_integer(C),list_to_integer(D)}, list_to_integer(PortSpec)};   
    _ -> 
      {{0,0,0,0}, 80}
  end,
  case hty_mounter:walk(Fspath, "resource", Mc) of
    {ok, Resources} ->
      Root = hty_union_resource:new(Resources),
      {ok, hty_http_server:new("http", Ip, Port, Root)};
    {error, Error} ->
      {error, {?MODULE, Error}}
  end.

protocol(This) -> This#hty_http_server.proto.

address(This) -> This#hty_http_server.ip.

port(This) -> This#hty_http_server.port.

root(This) -> This#hty_http_server.resource.

start(This) ->
  Ip = This#hty_http_server.ip,
  Port = This#hty_http_server.port,
  Resource = This#hty_http_server.resource,
  io:format("Trying to listen on ~p:~p", [Ip,Port]),
  case gen_tcp:listen(Port, [list,
    {packet, 0},
    {active, once},
    {ip, Ip},
    {reuseaddr, true},
    binary]) of
      {error, Error} ->
        io:format(", failed : ~p~n", [Error]),
        {error, Error};
      {ok, Listen} ->
        io:format(", ok~n"),
        Fun = fun() ->
          receive
            go -> loop_accept(Listen, Resource)
          end
        end,
        Accepter = spawn(Fun),
        ok = gen_tcp:controlling_process(Listen, Accepter),
        register(process_key(This),
          spawn(fun() ->
            loop_control(Listen, [])
          end)
        ),
        Accepter ! go,
        {ok, This}
  end.

loop_control(Listen, Sites) ->
  receive
    {stop, ReplyTo} ->
      gen_tcp:close(Listen),
      ReplyTo ! stopping;
    {sites, Sites} ->
      loop_control(Listen, Sites)
  end.

stop(This) ->
  Key = process_key(This),
  Key ! {stop, self()},
  receive
    stopping ->
      ok
  after
    60000 ->
      timeout
  end.

process_key(This) ->
	      list_to_atom(atom_to_list(hty_server) ++
	      " running on port " ++ integer_to_list(This#hty_http_server.port)).

loop_accept(Listen, Resource) ->
  case gen_tcp:accept(Listen) of
    {error, Error} ->
      error_logger:format("Accept failed ~p~n", [Error]);
    {ok, Socket} ->
      Handler = spawn(fun() ->
        receive
          go ->
            Htx = hty_parser:parse(Socket),
            Htx1 = hty_resource:invoke_handle(Htx, Resource),
            print_log(Htx1),
            hty_parser:respond(Socket, Htx1)
        end
      end),
      case gen_tcp:controlling_process(Socket, Handler) of
        ok ->
          Handler ! go;
        {error, Error} ->
          error_logger:format("Controlling process change failed ~p~n", [Error])
      end,
      loop_accept(Listen, Resource)
  end.


print_log(Htx) ->
  Log = hty_tx:log(Htx),
  Msg = lists:foldl(fun(Elem, Acc) ->
    {Category,Tstamp,Event,Data} = Elem,
    ["<log>", Category, $|, Tstamp, $|, Event, $|, Data, "</log>", 10|Acc]
  end, ["</tx>", 10], Log),
  io:format(user, [10, "<tx>", 10|Msg], []).
