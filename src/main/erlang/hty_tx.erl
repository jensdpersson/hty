-module(hty_tx, [Tx]).

-export([protocol/1, method/0, method/1, 
	 consume/1, consume/0, path/0,  path/1, path_below/0, path_below/1, matrix/1,
%	 next/0,
	 status/2, status/0, req_header/1, req_header/2, req_headers/0]).

-export([outs/0, sendfile/1, echo/1, clear/0, prolog/1]).
-export([rsp_header/2, rsp_headers/0]).

-export([recvdata/1, recvfile/2, recvform/2, 
	 buffer/1, unread/0, unread/1, flush/0]).

-export([process_entity/1]).

-export([ok/0,
	 not_found/0, 
	 method_not_allowed/1,
	 service_unavailable/0,
         see_other/0,
	 see_other/1,
	 temporary_redirect/1,
	 bad_request/0,
	 created/0,
	 conflict/0,
	 server_error/0,
	 server_error/1,
	 forbidden/0]).

-export([realm/0, realm/1]).
-export([principal/0, principal/1]).
-export([bind/2, bound/1]).

-export([queryparams/0, queryparams/1]).
-export([socketreader/1]).

-export([dispatch/1]).

-export([ndc_push/1, ndc_pop/0, ndc_peek/0]).

-export([log/0, log/2, mimemap/1]).

-include("hty_tx.hrl").

-type htx() :: {hty_tx, any()}.

%log(Category, Severity, Message) ->
%    LogEntry = {Category, Severity, hty_log:tstamp(), Message},
 %   hty_tx:new(Tx#tx{log=[LogEntry|Tx#tx.log]}).

mimemap("ogg") -> "audio/ogg";
mimemap("html") -> "text/html";
mimemap("xsl") -> "text/xsl";
mimemap("css") -> "text/css";
mimemap("javascript") -> "text/javascript";
mimemap("js") -> "text/javascript";
mimemap("png") -> "image/png";
mimemap("xml") -> "text/xml".

protocol(Proto) ->
	hty_tx:new(Tx#tx{proto=Proto}).

method() -> Tx#tx.method.

method(Method) -> hty_tx:new(Tx#tx{method=Method}).

path(PathZipper) -> 
    hty_tx:new(Tx#tx{path=PathZipper}).

path() ->
    Tx#tx.path.

path_below() -> 
    {_Above, Below} = Tx#tx.path, 
    lists:map(fun({Seg, _Matrix}) -> Seg; (Seg) -> Seg end, Below).

matrix(Key) ->
    hty_uri:matrix(Tx#tx.path, Key).

path_below(Below) ->
	{Above, _Below} = Tx#tx.path,
	hty_tx:new(Tx#tx{path={Above, Below}}).

consume() ->
	case Tx#tx.path of
		{Above, [Current|Below]} -> 
			{Current, hty_tx:new(Tx#tx{path={[Current|Above], Below}})};
		_ ->
			no
	end.

-spec consume(string()) -> htx() | no.
consume(Segment) ->
	case Tx#tx.path of
		{Above, [Segment|Below]} -> 
			hty_tx:new(Tx#tx{path={[Segment|Above], Below}});
		_ ->
			no
	end.

bind(Key, Value) ->
	Attrs = Tx#tx.attributes,
	hty_tx:new(Tx#tx{attributes=[{Key, Value}|Attrs]}).

bound(Key) ->
	case lists:keyfind(Key, 1, Tx#tx.attributes) of
		false ->
			no;
		{_, Value} ->
			{ok, Value}
	end.
			
queryparams() ->
	Tx#tx.queryparams.
queryparams(QueryParams) ->
	hty_tx:new(Tx#tx{queryparams=QueryParams}).

ok() -> status(200, "OK").
not_found() -> status(404, "Not Found").
method_not_allowed(Okmethods) -> 
    Htx1 = rsp_header("Allow", lists:concat(Okmethods)),
    Htx1:status(405, "Method Not Allowed").
service_unavailable() -> status(503, "Temporarily Unavailable").
server_error() ->
    status(500, "Internal Server Error").
server_error(Error) ->
    This1 = server_error(),
    hty_tx:new(This1:log("ServerError", Error)).
bad_request() ->
	status(400, "Bad Request").
created() ->
	status(201, "Created").
see_other() ->
        see_other(hty_uri:pack(Tx#tx.path)).
see_other(URI) ->
	Htx1 = rsp_header("Location", URI),
	Htx1:status(303, "See Other").
temporary_redirect(URI) ->
    	Htx1 = rsp_header("Location", URI),
        Htx1:status(307, "Temporary Redirect").
forbidden() ->
	status(403, "Forbidden").
conflict() ->
    status(409, "Conflict").

status() -> Tx#tx.status.
status(Code, Message) -> 
    Tx1 = log("status", Message),
    hty_tx:new(Tx1#tx{status={Code, Message}}).

req_header(Name, Value) -> hty_tx:new(Tx#tx{reqh=[{Name,Value}|Tx#tx.reqh]}).

req_header(Name) ->
	lists:flatmap(fun(Item) -> 
						 case Item of
							 {Name, Value} -> [Value];
							 _ -> []
						 end
				 end, Tx#tx.reqh).

req_headers() -> Tx#tx.reqh.

this() ->
	hty_tx:new(Tx).

flush() ->
	Binlist = lists:reverse(Tx#tx.buffered),
	Sink = Tx#tx.ondata,
	Laststate = Tx#tx.ondata_state,
	F = fun(Data, {State, Htx}) ->
				case Sink(Data, State, Htx) of
					{ok, NewState, Htx1} -> {next, {NewState, Htx1}};
					{no, Error} -> {break, Error}
				end
		end, 
	case hty_util:fold(F, {Laststate, this()}, Binlist) of
		{break, Error, Remains} ->
			hty_tx:new(Tx#tx{status=Error, buffered=Remains});
		{nobreak, {State, Htx2}} ->
			{hty_tx, Tx1} = Htx2,
			hty_tx:new(Tx1#tx{buffered=[], ondata_state=State})
	end.

unread() ->
	Tx#tx.unread.
unread(Leng) ->
	hty_tx:new(Tx#tx{unread=Leng}).
	
buffer(Data) ->
	Buffered = [Data|Tx#tx.buffered],
	Unread = unread() - case Data of
												eos -> 0;
												_ -> size(Data)
											end,
	hty_tx:new(Tx#tx{buffered=Buffered, unread=Unread}).

socketreader(SocketReader) ->
	hty_tx:new(Tx#tx{socketreader=SocketReader}).

process_entity(F) ->
	Htx = recvdata(F),
	Htx1 = Htx:flush(),
	SocketReader = Tx#tx.socketreader,
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

rsp_header(Name, Value) -> hty_tx:new(Tx#tx{rsph=[{Name,Value}|Tx#tx.rsph]}).

rsp_headers() -> Tx#tx.rsph.



recvdata(OnRecvData) -> hty_tx:new(Tx#tx{ondata=OnRecvData}).

recvform(FormSchema, Callback) ->
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
		   end).

recvfile(Spafs, Filepath) -> 
    Filter = hty_spaf:chain(Spafs),
    Fwrite = fun(IO, Data, ChainQ) -> 
		     case Filter(Data, ChainQ) of
			 {ok, ChainQ1, Data1} ->
			     case file:write(IO, Data1) of
				 ok -> {ok, {IO, ChainQ1}};
				 {error, Error} -> {no, Error}
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
			       {{error, Reason}, ok} -> {no, Reason};
			       {{error, Reason}, {error, Reason1}} ->
				   {no, {Reason, Reason1}}
			   end
		   end).

ndc_push(Frame) ->
    hty_tx:new(Tx#tx{ndc=[Frame|Tx#tx.ndc]}).

ndc_pop() ->
    [_|Ndc] = Tx#tx.ndc,
    hty_tx:new(Tx#tx{ndc=Ndc}).

ndc_peek() ->
    case Tx#tx.ndc of
        [Top|_] -> Top;
        [] -> {'/'}
    end.

log() ->
    Tx#tx.log.

log(Event, Data) ->
    Log = log(),
    Context = ndc_peek(),
    Category = atom_to_list(element(1, Context)),
    Tx#tx{log=[{Category,hty_log:tstamp(),Event,Data}|Log]}.

sendfile(Filename) ->
    Tx1 = log("sendfile", Filename),
    hty_tx:new(Tx1#tx{outs=[{file, Filename}|Tx1#tx.outs]}).

echo(IOList) -> hty_tx:new(Tx#tx{outs=[{data, IOList}|Tx#tx.outs]}).

prolog(IOList) -> 
    Outs1 = lists:reverse([{data, IOList}] ++ lists:reverse(Tx#tx.outs)),
    hty_tx:new(Tx#tx{outs=Outs1}).

clear() ->
    hty_tx:new(Tx#tx{outs=[]}).

outs() -> Tx#tx.outs.

dispatch(Resources) ->
    This = hty_tx:new(Tx),
    case hty_util:fold(fun(Resource, Htx) ->
                               try 
				   Htx1 = Htx:ndc_push(Resource),
				   Htx2 = Resource:handle(Htx1),
				   Htx3 = Htx2:ndc_pop(),
				   case Htx3:status() of
				       {404, _} -> 
					   {next, Htx3};
				       {405, _} ->
					   {next, Htx3};
				       _ ->
					   {break, Htx3}
				   end catch 
					   throw:Error -> 
					       Htx5 = Htx:ndc_push(Resource),
					       {break, Htx5:server_error(Error)}
				       end
		       end, This, Resources) of
	{break, Rsp, _} -> Rsp;
	{nobreak, Rsp} -> Rsp
    end.

realm() -> Tx#tx.realm.
realm(Realm) -> hty_tx:new(Tx#tx{realm=Realm}).

principal() -> Tx#tx.principal.
principal(Principal) -> hty_tx:new(Tx#tx{principal=Principal}).
