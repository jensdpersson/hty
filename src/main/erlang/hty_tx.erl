-module(hty_tx, [Tx]).

-export([protocol/1, method/0, method/1, 
				 next_in_path/0, path/1, path_below/0, 
				 status/2, status/0, req_header/1, req_header/2]).

-export([outs/0, sendfile/1, echo/1]).
-export([rsp_header/2, rsp_headers/0]).

-export([recvdata/1, recvfile/2, buffer/1, flush/0]).

-export([not_found/0, 
				 method_not_allowed/1,
				 service_unavailable/0]).

-include("hty_tx.hrl").



protocol(Proto) ->
	hty_tx:new(Tx#tx{proto=Proto}).

method() ->	Tx#tx.method.

method(Method) -> hty_tx:new(Tx#tx{method=Method}).

path(Path) -> hty_tx:new(Tx#tx{path=Path}).

path_below() -> 
	{_Above, Below} = Tx#tx.path, 
	Below.

next_in_path() ->
	case Tx#tx.path of
		{Above, [Current|Below]} -> 
			hty_tx:new(Tx#tx{path={[Current|Above], Below}});
		_ ->
			no
	end.

not_found() -> status(404, "Not Found").
method_not_allowed(Okmethods) -> 
	Htx1 = rsp_header("Allow", lists:concat(Okmethods)),
	Htx1:status(405, "Method Not Allowed").
service_unavailable() -> status(503, "Temporarily Unavailable").


status() -> Tx#tx.status.
status(Code, Message) -> hty_tx:new(Tx#tx{status={Code, Message}}).

req_header(Name, Value) -> hty_tx:new(Tx#tx{reqh=[{Name,Value}|Tx#tx.reqh]}).

req_header(Name) ->
	lists:filter(fun(Item) -> 
						 case Item of
							 Name -> true;
							 _ -> false
						 end
				 end, Tx#tx.reqh).

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

recvfile(Spafchain, Filepath) -> 
	Filter = hty_spaf:chain(Spafchain),
	recvdata(fun(Data, State) ->
								case Data of
									eos -> case State of
													q0 -> {ok, done};
													{open, File} -> 
														case file:close(File) of
															ok -> {ok, done};
															{error, Error} ->
																{no, Error}
														end;
													done -> {ok, done}
												end;
									_ ->
										ErrorOrIO = case State of 
													 q0 ->
														 Opts = [raw, write, delayed_write],
														 %file:open has the same type as we use here
														 % {ok, IO}|{error,Error}
														 file:open(Filepath, Opts);
													 {open, Openfile} ->
														 {ok, Openfile};
													 done	->
														 {error, more_data_in_state_done}
												 end,
										case ErrorOrIO of
											{ok, IO} ->
												case Filter(Data) of
													{ok, Data1} ->
														case file:write(IO, Data1) of
															ok -> {ok, {open, IO}};
															{error, Error} -> {no, Error}
														end;
													{no, Error} -> {no, Error}
												end;
											{error, Error} -> {no, Error}
										end
								end
					 end).

sendfile(Filename) -> hty_tx:new(Tx#tx{outs=[{file, Filename}|Tx#tx.outs]}).

echo(IOList) -> hty_tx:new(Tx#tx{outs=[{data, IOList}|Tx#tx.outs]}).

outs() -> Tx#tx.outs.