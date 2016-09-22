-module(hty_tx).



-export([protocol/2,
	 method/1,
	 method/2,
	 consume/2,
	 consume/1,
	 path/1,
	 path/2,
	 path_above/1,
	 path_below/1,
	 path_below/2,
	 matrix/2,
	 status/3,
	 status/1,
	 req_header/2,
	 req_header/3,
	 req_headers/1]).

-export([outs/1,
	 sendfile/2,
	 echo/2,
	 clear/1,
	 prolog/2,
	 copy/2]).

-export([rsp_header/2,
	 rsp_header/3,
	 rsp_headers/1,
	 rsp_entity/1]).

-export([recvdata/2,
	 recvfile/3,
	 recvform/3,
	 buffer/2,
	 unread/1,
	 unread/2,
	 flush/1]).

-export([process_entity/2]).

-export([ok/1,
	 not_found/1,
	 method_not_allowed/2,
	 service_unavailable/1,
	 see_other/1,
	 see_other/2,
	 not_modified/1,
	 temporary_redirect/2,
	 bad_request/1,
	 created/1,
	 conflict/1,
	 server_error/1,
	 server_error/2,
	 forbidden/1,
	 gone/1]).

-export([commit/1, committed/1]).

-export([realm/1,
	 realm/2]).
-export([principal/1,
	 principal/2]).
-export([bind/3,
	 bound/2]).

-export([queryparam/2, queryparams/1, queryparams/2]).
-export([socketreader/2]).

-export([dispatch/2]).

-export([ndc_push/2,
	 ndc_pop/1,
	 ndc_peek/1]).

-export([log/1,
	 log/3,
	 mimemap/2]).

-export([new/0]).

-export([peer/1, peer/2]).

-record(hty_tx, {
		peer={notset, nil, nil},
	  proto="HTTP/1.1",
	  method='GET',
	  path={[], []},
	  status={200, "OK"},
	  reqh=[],
	  buffered=[],
	  ondata=fun(_Data, _State, Htx) -> {ok, noop_state, Htx} end,
	  ondata_state=q0,
	  rsph=[],
	  outs=[],
	  principal= <<"guest">>,
	  realm=hty_empty_realm,
	  attributes=[],
	  queryparams=[],
	  socketreader=no,
	  ndc=[],
	  log=[],
	  unread=0,
	  committed=false}).

-type htx() :: #hty_tx{}.
-export_type([htx/0]).

-spec new() -> htx().
new() ->
    #hty_tx{}.

mimemap("ogg", _This) -> "audio/ogg";
mimemap("html", _This) -> "text/html";
mimemap("xsl", _This) -> "text/xsl";
mimemap("css", _This) -> "text/css";
mimemap("javascript", _This) -> "text/javascript";
mimemap("js", _This) -> "text/javascript";
mimemap("png", _This) -> "image/png";
mimemap("jpg", _This) -> "image/jpeg";
mimemap("xml", _This) -> "text/xml";
mimemap("ico", _This) -> "image/vnd.microsoft.icon";
mimemap("txt", _This) -> "text/plain";
mimemap(_, _) -> "application/octetstream".

protocol(Proto, This) ->
	This#hty_tx{proto=Proto}.

method(This) -> This#hty_tx.method.

method(Method, This) -> This#hty_tx{method=Method}.

path(PathZipper, This) ->
    This#hty_tx{path=PathZipper}.

path(This) ->
    This#hty_tx.path.

path_below(This) ->
    {_Above, Below} = This:path(),
    lists:map(fun({Seg, _Matrix}) -> Seg; (Seg) -> Seg end, Below).

path_above(This) ->
    {Above, _Below} = This:path(),
    lists:map(fun({Seg, _Matrix}) -> Seg; (Seg) -> Seg end, Above).

matrix(Key, This) ->
    hty_uri:matrix(This:path(), Key).

path_below(Below, This) ->
	{Above, _Below} = This:path(),
	This#hty_tx{path={Above, Below}}.

consume(This) ->
    case This:path() of
	{Above, [Current|Below]} ->
	    {Current, This#hty_tx{path={[Current|Above], Below}}};
	_ ->
	    no
    end.

consume(Segment, This) ->
    case This:path() of
	{Above, [Segment|Below]} ->
	    This#hty_tx{path={[Segment|Above], Below}};
	_ ->
	    no
    end.

bind(Key, Value, This) ->
    Attrs = This#hty_tx.attributes,
    This#hty_tx{attributes=[{Key, Value}|Attrs]}.

bound(Key, This) ->
  case lists:keyfind(Key, 1, This#hty_tx.attributes) of
    false ->
      no;
    {_, Value} ->
      {ok, Value}
  end.

queryparams(This) ->
  This#hty_tx.queryparams.

queryparams(QueryParams, This) ->
  This#hty_tx{queryparams=QueryParams}.

queryparam(Param, This) ->
  lists:filtermap(fun(Entry) ->
    case Entry of
      {Param, Value} ->
        {true, Value};
      _ -> false
    end
  end, This#hty_tx.queryparams).

-spec ok(htx()) -> htx().
ok(This) -> status(200, "OK", This).

-spec not_found(htx()) -> htx().
not_found(This) -> status(404, "Not Found", This).

-spec method_not_allowed(list(string()), htx()) -> htx().
method_not_allowed(Okmethods, This) ->
    Htx1 = This:rsp_header("Allow", lists:concat(Okmethods)),
    Htx1:status(405, "Method Not Allowed").

service_unavailable(This) -> status(503, "Temporarily Unavailable", This).
server_error(This) ->
    status(500, "Internal Server Error", This).
server_error(Error, This) ->
    (server_error(This)):log("ServerError", Error).
bad_request(This) ->
    status(400, "Bad Request", This).
created(This) ->
    status(201, "Created", This).
see_other(This) ->
    see_other(hty_uri:pack(This:path()), This).
see_other(URI, This) ->
    Htx1 = rsp_header("Location", URI, This),
    Htx1:status(303, "See Other").
not_modified(This) ->
    status(304, "Not Modified", This).
temporary_redirect(URI, This) ->
    Htx1 = rsp_header("Location", URI, This),
    Htx1:status(307, "Temporary Redirect").
forbidden(This) ->
    status(403, "Forbidden", This).
gone(This) ->
  status(510, "Gone", This).

-spec conflict(htx()) -> htx().
conflict(This) ->
    status(409, "Conflict", This).

-spec status(htx()) -> {integer(), string()}.
status(This) -> This#hty_tx.status.

-spec status(Code::integer(), Message::string(), htx()) -> htx().
status(Code, Message, This) ->
    This1 = log("status", Message, This),
    This1#hty_tx{status={Code, Message}}.

req_header(Name, Value, This) -> This#hty_tx{reqh=[{Name,Value}|This#hty_tx.reqh]}.

req_header(Name, This) ->
    lists:flatmap(fun(Item) ->
			  case Item of
			      {Name, Value} -> [Value];
			      _ -> []
			  end
		  end, This#hty_tx.reqh).

req_headers(This) -> This#hty_tx.reqh.

flush(This) ->
  Binlist = lists:reverse(This#hty_tx.buffered),
  Sink = This#hty_tx.ondata,
  Laststate = This#hty_tx.ondata_state,
  F = fun(Data, {State, Htx}) ->
    case Sink(Data, State, Htx) of
      {ok, NewState, Htx1} -> {next, {NewState, Htx1}};
      {no, Error} -> {break, Error}
    end
  end,
  case hty_util:fold(F, {Laststate, This}, Binlist) of
    {break, Error, Remains} ->
      #hty_tx{status=Error, buffered=Remains};
    {nobreak, {State, Htx2}} ->
      %{hty_tx, Tx1} = Htx2,
			Tx1 = Htx2,
      Tx1#hty_tx{buffered=[], ondata_state=State}
  end.

unread(This) ->
    This#hty_tx.unread.
unread(Leng, This) ->
    This#hty_tx{unread=Leng}.

buffer(Data, This) ->
    Buffered = [Data|This#hty_tx.buffered],
    Unread = This:unread() - case Data of
			    eos -> 0;
			    _ -> size(Data)
			end,
    This#hty_tx{buffered=Buffered, unread=Unread}.

socketreader(SocketReader, This) ->
    This#hty_tx{socketreader=SocketReader}.

process_entity(F, This) ->
    Htx = recvdata(F, This),
    Htx1 = Htx:flush(),
    SocketReader = Htx1#hty_tx.socketreader,
    pump(SocketReader, Htx1).

pump(SocketReader, Htx) ->
  case SocketReader:recv(Htx:unread()) of
    timeout ->
      {no, timeout};
    {done, _Reason} ->
      Htx1 = Htx:buffer(eos),
      Htx1:flush();
    {data, Data, SocketReader1} ->
      Htx1 = Htx:buffer(Data),
      Htx2 = Htx1:flush(),
      pump(SocketReader1, Htx2)
  end.

rsp_header(Name, This) ->
    case lists:keyfind(Name, 1, This#hty_tx.rsph) of
	false ->
	    no;
	{Name, Value} ->
	    {ok, Value}
    end.

rsp_header(Name, Value, This) ->
    This#hty_tx{rsph=[{Name,Value}|This#hty_tx.rsph]}.

rsp_headers(This) -> This#hty_tx.rsph.

rsp_entity(This) ->
    lists:foldl(fun({data, Data}, Acc) ->
			DataBin = list_to_binary(Data),
			<<DataBin/binary, Acc/binary>>;
		   ({file, File}, Acc) ->
			case file:read_file(File) of
			    {ok, Binary} ->
				<<Binary/binary, Acc/binary>>;
			    {error, Reason} ->
				io:format("Failed (~p) reading file [~p] ~n", [Reason, File]),
				""
			end
		end, <<"">>, outs(This)).

recvdata(OnRecvData, This) -> This#hty_tx{ondata=OnRecvData}.

recvform(FormSchema, Callback, This) ->
    Spafs = [fun hty_formurlencoded_spaf:parse/2,
	     hty_spaf:binder(FormSchema)],
    Chain = hty_spaf:chain(Spafs),
    process_entity(fun(Data, State, Htx) ->
			   case Chain(Data, State) of
			       {ok, State1, Out} ->
				   case Data of
				       eos ->
					   case Callback(Out, Htx) of
					       {ok, Htx1} ->
						   {ok, State1, Htx1};
					       {no, Error} ->
						   {no, Error}
					   end;
				       _ ->
					   {ok, State1, Htx}
				   end;
			       {no, Error} ->
				   {no, Error}
			   end
		   end, This).

-spec recvfile(Spafs::list(), Filepath::string(), htx()) -> htx().
recvfile(Spafs, Filepath, This) ->
  Filter = hty_spaf:chain(Spafs),
  Fwrite = fun(IO, Data, ChainQ) ->
    case Filter(Data, ChainQ) of
      {ok, ChainQ1, Data1} ->
				io:format("Data1 is ~p~n", [Data1]),
        case Data1 of
          [eos] -> {ok, {IO, ChainQ1}};
          _ ->
            case file:write(IO, Data1) of
              ok -> {ok, {IO, ChainQ1}};
              {error, Error} -> {no, Error}
            end
        end;
      {no, Error} -> {no, Error}
    end
  end,
  process_entity(fun(Data, State, Htx) ->
    {Fd, WriteRes} = case State of
      q0 ->
        Opts = [raw, write, delayed_write],
        case file:open(Filepath, Opts) of
          {ok, IODevice} -> {IODevice, Fwrite(IODevice, Data, q0)};
          {error, E} -> {nofilehandle, {no, E}}
        end;
      {IOFile, Q} -> {IOFile, Fwrite(IOFile, Data, Q)}
    end,
    CloseRes = case Data of
      eos ->
        case Fd of
          nofilehandle -> {no, nofilehandle};
          File -> file:close(File)
        end;
      _ -> ok
    end,
    case {WriteRes, CloseRes} of
      {{ok, {IO, Q1}}, ok} -> {ok, {IO, Q1}, Htx};
      {{ok, _}, {error, Reason}} -> {no, Reason};
      {{no, Reason}, _} -> {no, Reason}%;
      %{{error, Reason}, ok} -> {no, Reason};
      %{{error, Reason}, {error, Reason1}} ->
      %  {no, {Reason, Reason1}}
    end
  end, This).

ndc_push(Frame, This) ->
    This#hty_tx{ndc=[Frame|This#hty_tx.ndc]}.

ndc_pop(This) ->
    [_|Ndc] = This#hty_tx.ndc,
    This#hty_tx{ndc=Ndc}.

ndc_peek(This) ->
    case This#hty_tx.ndc of
        [Top|_] -> Top;
        [] -> {'/'}
    end.

-spec log(htx()) -> list().
log(This) ->
    This#hty_tx.log.

log(Event, Data, This) ->
    Log = log(This),
    Context = ndc_peek(This),
		%io:format("APA ~p", [Context]),
    Category = atom_to_list(element(1, Context)),
    This#hty_tx{log=[{Category,hty_date:format(hty_date:now()),Event,Data}|Log]}.

-spec sendfile(Filename::string(), htx()) -> htx().
sendfile(Filename, This) ->
    This1 = log("sendfile", Filename, This),
    This1#hty_tx{outs=[{file, Filename}|This#hty_tx.outs]}.

-spec echo(IOList::iolist(), htx()) -> htx().
echo(IOList, This) -> This#hty_tx{outs=[{data, IOList}|This#hty_tx.outs]}.

copy(Htx, This) ->
    This#hty_tx{outs=(Htx:outs() ++ This#hty_tx.outs)}.

prolog(IOList, This) ->
    Outs1 = lists:reverse([{data, IOList}] ++ lists:reverse(This#hty_tx.outs)),
    This#hty_tx{outs=Outs1}.

-spec clear(htx()) -> htx().
clear(This) ->
    This#hty_tx{outs=[]}.

outs(This) -> This#hty_tx.outs.

-spec dispatch(Resource::tuple() | [Resource::tuple()], htx()) -> htx().
dispatch(Resource, This) when not is_list(Resource) ->
    dispatch([Resource], This);
dispatch(Resources, This) ->
  F = fun(Resource, Htx) ->
    try
      Htx1 = Htx:ndc_push(Resource),
			io:format("dispatching to ~p~n", [Resource]),
      Htx2 = Resource:handle(Htx1),
      Htx3 = Htx2:ndc_pop(),
      case Htx3:committed() of
        false -> {next, Htx3};
        true ->	{break, Htx3}
      end
    catch
      throw:Error ->
        Htx5 = Htx:ndc_push(Resource),
        {break, Htx5:server_error(Error)};
      error:Error ->
        Trace = erlang:get_stacktrace(),
        io:format("Dispatch caught ~p :: ~p~n", [Error, Trace]),
        {break, Htx:server_error(atom_to_list(Error))}
    end
  end,
  case hty_util:fold(F, This, Resources) of
    {break, Rsp, _} -> Rsp;
    {nobreak, Rsp} -> Rsp
  end.

realm(This) -> This#hty_tx.realm.
realm(Realm, This) -> This#hty_tx{realm=Realm}.

principal(This) -> This#hty_tx.principal.
principal(Principal, This) -> This#hty_tx{principal=Principal}.

-spec commit(htx()) -> htx().
commit(This) -> This#hty_tx{committed=true}.

-spec committed(htx()) -> true | false.
committed(This) -> This#hty_tx.committed.

-spec peer({(ipv4 | ipv6 | other), Address::any(), Port::integer()}) -> htx().
peer(Peer, This) ->
	This#hty_tx{peer=Peer}.

peer(This) ->
	This#hty_tx.peer.
