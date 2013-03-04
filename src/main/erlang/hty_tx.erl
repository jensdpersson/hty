-module(hty_tx, [Tx]).

-export([protocol/1, method/0, method/1, 
				 consume/1, consume/0, path/1, path_below/0, 
				 status/2, status/0, req_header/1, req_header/2, req_headers/0]).

-export([outs/0, sendfile/1, echo/1]).
-export([rsp_header/2, rsp_headers/0]).

-export([recvdata/1, recvfile/2, recvform/2, buffer/1, flush/0]).

-export([ok/0,
				 not_found/0, 
				 method_not_allowed/1,
				 service_unavailable/0,
				 see_other/1,
				 bad_request/0,
				 server_error/0,
				 forbidden/0]).

-export([realm/0, realm/1]).
-export([loggedin/0, loggedin/1]).
-export([bind/2, bound/1]).

-export([queryparams/0, queryparams/1]).

-export([dispatch/1]).

-include("hty_tx.hrl").

-type htx() :: {hty_tx, any()}.

protocol(Proto) ->
	hty_tx:new(Tx#tx{proto=Proto}).

method() ->	Tx#tx.method.

method(Method) -> hty_tx:new(Tx#tx{method=Method}).

path(Path) -> hty_tx:new(Tx#tx{path=Path}).

path_below() -> 
	{_Above, Below} = Tx#tx.path, 
	Below.

consume() ->
	case Tx#tx.path of
		{Above, [Current|Below]} -> 
			{Current, hty_tx:new(Tx#tx{path={[Current|Above], Below}})};
		_ ->
			no
	end.

-spec consume(string()) -> htx() | no.
consume(Segment) ->
	io:format("NextInPath(~p=?~p)~n",[Segment, Tx#tx.path]),
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
bad_request() ->
	status(400, "Bad Request").
see_other(URI) ->
	Htx1 = rsp_header("Location", URI),
	Htx1:status(303, "See Other").
forbidden() ->
	status(403, "Forbidden").

status() -> Tx#tx.status.
status(Code, Message) -> 
	io:format("Status=~p~n", [Message]),
	hty_tx:new(Tx#tx{status={Code, Message}}).

req_header(Name, Value) -> hty_tx:new(Tx#tx{reqh=[{Name,Value}|Tx#tx.reqh]}).

req_header(Name) ->
	lists:flatmap(fun(Item) -> 
						 case Item of
							 {Name, Value} -> [Value];
							 _ -> []
						 end
				 end, Tx#tx.reqh).

req_headers() -> Tx#tx.reqh.

flush() ->
	Binlist = lists:reverse([eos|Tx#tx.buffered]),
	Sink = Tx#tx.ondata,
	Laststate = Tx#tx.ondata_state,
	F = fun(Data, State) ->
				case Sink(Data, State) of
					{ok, NewState} -> {next, NewState};
					{no, Error} -> {break, Error}
				end
		end, 
	case hty_util:fold(F, Laststate, Binlist) of
		{break, Error, Remains} ->
			hty_tx:new(Tx#tx{status=Error, buffered=Remains});
		{nobreak, State} ->
			hty_tx:new(Tx#tx{buffered=[], ondata_state=State})
	end.
	

buffer(Data) ->
	Buffered = [Data|Tx#tx.buffered],
	hty_tx:new(Tx#tx{buffered=Buffered}).

rsp_header(Name, Value) -> hty_tx:new(Tx#tx{rsph=[{Name,Value}|Tx#tx.rsph]}).

rsp_headers() -> Tx#tx.rsph.



recvdata(OnRecvData) -> hty_tx:new(Tx#tx{ondata=OnRecvData}).

recvform(FormSchema, Callback) ->
	Spafs = [fun hty_formurlencoded_spaf:parse/2,
					 hty_spaf:binder(FormSchema)],
	Chain = hty_spaf:chain(Spafs),
	recvdata(fun(Data, State) ->
								case Chain(Data, State) of
									{ok, State1, Out} ->
										case Data of
											eos ->
												case Callback(Out) of
													ok ->
														{ok, State1};
													{no, Error} ->
														{no, Error}
												end;
											_ ->
												{ok, State1}
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
	recvdata(fun(Data, State) ->
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
									{{ok, {IO, Q1}}, ok} -> {ok, {IO, Q1}};
									{{ok, _}, {error, Reason}} -> {no, Reason};
									{{error, Reason}, ok} -> {no, Reason};
									{{error, Reason}, {error, Reason1}} ->
										{no, {Reason, Reason1}}
								end
					 end).



sendfile(Filename) -> hty_tx:new(Tx#tx{outs=[{file, Filename}|Tx#tx.outs]}).

echo(IOList) -> hty_tx:new(Tx#tx{outs=[{data, IOList}|Tx#tx.outs]}).

outs() -> Tx#tx.outs.

dispatch(Resources) ->
	This = hty_tx:new(Tx),
	io:format("dispatch(~p)~n", [Resources]),
	case hty_util:fold(fun(Resource, Htx) ->
																Htx1 = Resource:handle(This),
																case Htx1:status() of
																	{404, _} -> 
																		{next, Htx};
																	_ ->
																		{break, Htx1}
																end
													 end, This:not_found(), Resources) of
		{break, Rsp, _} -> Rsp;
		{nobreak, Rsp} -> Rsp
	end.

realm() -> Tx#tx.realm.
realm(Realm) -> hty_tx:new(Tx#tx{realm=Realm}).

loggedin() -> Tx#tx.loggedin.
loggedin(Loggedin) -> hty_tx:new(Tx#tx{loggedin=Loggedin}).
