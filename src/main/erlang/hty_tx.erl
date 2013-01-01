-module(hty_tx, [Proto, Method, Path, Status, Reqh, Reqe, Rsph, Outs]).

-export([protocol/1, method/0, method/1, next_in_path/0, path/1, path_below/0, status/1, status/0, req_header/1, req_header/2]).

-export([req_entity/0, req_entity/1, outs/0, sendfile/1, echo/1]).
-export([rsp_header/2, rsp_headers/0]).

-export([not_found/0, method_not_allowed/1]).

protocol(Proto1) ->
	hty_tx:new(Proto1, Method, Path, Status, Reqh, Reqe, Rsph, Outs).

method() ->
	Method.
method(Method1) ->
	hty_tx:new(Proto, Method1, Path, Status, Reqh, Reqe, Rsph, Outs).

path(Path1) ->
	hty_tx:new(Proto, Method, Path1, Status, Reqh, Reqe, Rsph, Outs).

path_below() -> 
	{_Above, Below} = Path, 
	Below.

next_in_path() ->
	case Path of
		{Above, [Current|Below]} -> 
			hty_tx:new(Proto, Method, {[Current|Above], Below}, Status, Reqh, Reqe, Rsph, Outs);
		_ ->
			no
	end.

not_found() -> status("404 Not Found").
method_not_allowed(Okmethods) -> 
	Htx1 = rsp_header("Allow", Okmethods),
	Htx1:status("405 Method Not Allowed").

status() -> Status.
status(Status1) ->
	hty_tx:new(Proto, Method, Path, Status1, Reqh, Reqe, Rsph, Outs).


req_header(Name, Value) ->
	hty_tx:new(Proto, Method, Path, Status, [{Name,Value}|Reqh], Reqe, Rsph, Outs).

req_header(Name) ->
	lists:filter(fun(Item) -> 
						 case Item of
							 Name -> true;
							 _ -> false
						 end
				 end, Reqh).

req_entity(Entity) ->
	hty_tx:new(Proto, Method, Path, Status, Reqh, Entity, Rsph, Outs).

req_entity() -> Reqe.

rsp_header(Name, Value) ->
	hty_tx:new(Proto, Method, Path, Status, Reqh, Reqe, [{Name,Value}|Rsph], Outs).

rsp_headers() -> Rsph.

sendfile(Filename) ->
  	hty_tx:new(Proto, Method, Path, Status, Reqh, Reqe, Rsph, [{file, Filename}|Outs]).

echo(IOList) ->
	hty_tx:new(Proto, Method, Path, Status, Reqh, Reqe, Rsph, [{data, IOList}|Outs]).

outs() -> Outs.