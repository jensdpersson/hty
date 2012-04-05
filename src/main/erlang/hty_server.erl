-module(hty_server).
-export([start/0, listen/2, serve/2]).

-include("http.hrl").


-record(server_state, {ports=[], engines=[], sites=[]}).

%External API

start() ->
    Dispatcher = spawn(fun() -> loop_dispatch(#server_state{}) end),
    register(?MODULE, Dispatcher),
    {ok, started}.


%@doc Ip is an ipv4 address as a 4-tuple of bytes, Port is an integer
listen(Ip, Port) ->
    io:format("Trying to listen on ~p:~p", [Ip,Port]),
    case gen_tcp:listen(Port, [list, {packet, 0}, {active, once}, {ip, Ip}, {reuseaddr, true}]) of
        {error, Error} ->
            io:format(", failed : ~p~n", [Error]),
            unregister(?MODULE),
            {error, Error};
        {ok, Listen} ->
            io:format(", ok~n"),
	    Fun = fun() -> 
			  receive 
			      go -> loop_accept(Listen) 
			  end 
		  end,
	    Accepter = spawn(Fun),
	    ok = gen_tcp:controlling_process(Listen, Accepter),
	    Accepter ! go,
            ok
    end.

serve(_SiteInfo, _Engine) -> log('Preparing site').

prepare(EngineName, Engines, Params, ReplyTo) ->
    case lists:keysearch(EngineName, 1, Engines) of
	false -> ReplyTo ! {error, {site_engine_not_loaded, EngineName}};
	{_, {_, Engine}} ->
	    case Engine:site(Params) of
		{ok, Site} -> ?MODULE ! {prepared, Site, ReplyTo};
		Error -> ReplyTo ! {error, Error}
	    end
    end.

load_engine(EngineName, EngineModule, ReplyTo) ->
    %Do we need to do code:load() or similar here?
    case EngineModule:start() of
	{ok, Engine} -> 
	    ?MODULE ! {engine_loaded, EngineName, Engine, ReplyTo};
	{error, Error} -> 
	    ReplyTo ! {error, Error}
    end.

%Loops
loop_accept(Listen) ->
    case gen_tcp:accept(Listen) of
	{error, Error} -> log(Error);
	{ok, Socket} ->
	    Handler = spawn(fun() -> receive {sites, Sites} -> handle(Socket, Sites) end end),
	    ok = gen_tcp:controlling_process(Socket, Handler),
	    ?MODULE ! {request, Handler},
	    loop_accept(Listen)
    end.

loop_dispatch(ServerState) ->
    receive
        {request, Handler} ->
            Handler ! {sites, ServerState#server_state.sites},
            loop_dispatch(ServerState);
        {mount, Params, ReplyTo} -> 
	    log('preparing'),
	    spawn(fun() -> prepare(donner, ServerState#server_state.engines, Params, ReplyTo) end),
	    loop_dispatch(ServerState);
	{prepared, Site, ReplyTo} ->
	    ReplyTo ! {ok, mounted},
	    {Replaced, Sites1} = lists:foldl(fun(Item, {Replaced, Rv}) -> 
			case Replaced of
			    true -> {true, [Item|Rv]};
			    false ->
				case Item#site.host == Site#site.host of
				    true -> io:format("Remounting ~p~n", [Item#site.host]),
						      {true, [Site|Rv]};
				    false -> {Replaced, [Item|Rv]}
				end
			end
		end, {false, []}, ServerState#server_state.sites),
	    case Replaced of 
		true -> loop_dispatch(ServerState#server_state{sites=Sites1});
		false -> loop_dispatch(add(site, Site, ServerState))
	    end;
	{status, ReplyTo} ->
	    io:format("Sending status to ~p~n", [ReplyTo]),
	    Stati = {
	      {sites, lists:map(fun(#site{host=Host, status=Status}) 
				   -> {Host,Status}
				end, ServerState#server_state.sites)},
	      {engines, lists:map(fun({EngineName, _EngineModule}) 
                                        -> EngineName end,
                                    ServerState#server_state.engines)},
              {ports, ServerState#server_state.ports}
	     },
	    log(Stati),
	    log(ReplyTo),
	    ReplyTo ! {status, Stati}, 
	    loop_dispatch(ServerState);
	{stop, ReplyTo} -> ReplyTo ! {stopping}, init:stop();
	{load_engine, EngineName, EngineModule, ReplyTo} ->
	    spawn(fun() -> load_engine(EngineName, EngineModule, ReplyTo) end),
	    loop_dispatch(ServerState);
	{engine_loaded, EngineName, Engine, ReplyTo} ->
	    ReplyTo ! {ok, loaded},
	    loop_dispatch(add(engine, {EngineName, Engine}, ServerState));
        {listen, [Ip, Port], ReplyTo} ->
            case listen(Ip, Port) of
                ok -> ReplyTo ! {ok, listening, Ip, Port};
                {error, Error} -> ReplyTo ! {error, Error}
            end,
            loop_dispatch(add(port, {Ip, Port}, ServerState));
	SomeMessage -> 
            io:format("Unknown message ~p~n", [SomeMessage]),
            loop_dispatch(ServerState)
    end.

add(port, Port, State) ->
    Ports = State#server_state.ports,
    State#server_state{ports=[Port|Ports]};
add(site, Site, State) ->
    Sites = State#server_state.sites,
    State#server_state{sites=[Site|Sites]};
add(engine, Engine, State) ->
    Engines = State#server_state.engines,
    State#server_state{engines=[Engine|Engines]}.

tstamp() ->
    {{Y,M,D},{H,Mi,S}} = erlang:localtime(),
    integer_to_list(Y) ++ "-" ++
    integer_to_list(M) ++ "-" ++
    integer_to_list(D) ++ "T" ++
    integer_to_list(H) ++ ":" ++
    integer_to_list(Mi) ++ ":" ++
    integer_to_list(S).

%Worker functions
handle(Socket, Sites) ->
    Req = parse(Socket),
    Host = header(Req, 'Host'),
    io:format("~p : ~p ~n", [tstamp(), Req]),
    Filter = fun(Site) -> 
		     io:format("Host comparison ~p =? ~p ~n", [Site#site.host, Host]),
		     Site#site.host == Host
	     end,
    case lists:filter(Filter, Sites) of
        [Site] -> 
            F = Site#site.root,
            respond(Socket, F(Req));
        [] -> 
	    respond(Socket, #http{status="404 NOT FOUND"})
    end.

header(Req, Name) ->
    case lists:keysearch(Name, 1, Req#http.headers) of
        {value, {_Key,Value}} -> Value;
        false -> []
    end.

log(Msg) -> io:format("~p~n", [Msg]).

pack_status({Code, Message}) when is_integer(Code) -> integer_to_list(Code) ++ " " ++ Message;
pack_status(Message) -> Message.

pack_header(HeaderName, HeaderValue) when is_atom(HeaderName) ->
    atom_to_list(HeaderName) ++ ": " ++ HeaderValue;
pack_header(HeaderName, HeaderValue) -> HeaderName ++ ": " ++ HeaderValue.

respond(Socket, #http{} = Rsp) ->
    send_line(Socket, "HTTP/1.1 " ++ pack_status(Rsp#http.status)),
    send_line(Socket, "Server: haughty 0.2"),
    lists:foreach(fun({HName, HValue}) -> send_line(Socket, pack_header(HName, HValue)) end, Rsp#http.headers),
    send_line(Socket, ""),
    gen_tcp:send(Socket, Rsp#http.entity).

send_line(Socket, Line) -> gen_tcp:send(Socket,  Line ++ "\r\n").

parse(Socket) -> parse_loop(#http{}, [], fun method_parser/2, Socket).

parse_loop(Req, Unparsed, Parser, Socket) ->
    case Parser(Req, Unparsed) of
        {Req1, Unparsed1, Parser1} -> parse_loop(Req1, Unparsed1, Parser1, Socket);
        more -> recv(Req, Unparsed, Parser, Socket);
        {done, Req1} -> Req1;
        {error, Error} -> {error, Error}
    end.

recv(Req, Unparsed, Parser, Socket) ->
    receive
       {tcp, Socket1, Data} ->
           inet:setopts(Socket, [{active, once}]),
           Unparsed1 = Unparsed ++ Data,
           parse_loop(Req, Unparsed1, Parser, Socket1);
       {tcp_closed, _} -> Req
    end.


method_parser(Req, Data) -> 
    case token(Data, 32) of
        {token, Method, Rest} -> {Req#http{method=canonical_method(Method)},Rest,fun path_parser/2};
        {more, _Token} -> more
    end.

canonical_method("GET") -> 'GET';
canonical_method("POST") -> 'POST';
canonical_method("PUT") -> 'PUT';
canonical_method("DELETE") -> 'DELETE';
canonical_method("OPTIONS") -> 'OPTIONS';
canonical_method("HEAD") -> 'HEAD';
canonical_method(Method) -> Method.

path_parser(Req, Data) ->
    case token(Data, 32) of
        {token, Path, Rest} -> {Req#http{path=Path},Rest,fun protocol_parser/2};
        {more, _Token} -> more
    end.

protocol_parser(Req, Data) ->
    case line(Data) of
        {token, Protocol, Rest} -> {Req#http{protocol=Protocol},Rest,fun header_parser/2};
        {more, _Token} -> more
    end.


header_parser(Req, [13,10|Data]) -> {Req, Data, fun body_parser/2};
header_parser(Req, [13|Data]) -> {Req, Data, fun body_parser/2};
header_parser(Req, [10|Data]) -> {Req, Data, fun body_parser/2};
header_parser(Req, Data) ->
    case token(Data, $:) of
        {token, Name, Data1} ->
            case line(Data1) of
               {token, Value, Data2} -> {
                   Req#http{headers=[{list_to_atom(Name),string:strip(Value)}|Req#http.headers]},
                                         Data2, fun header_parser/2};
               {more, _Token} -> more
            end;
        {more, _Token} -> more
    end.

body_parser(Req, Data) ->
    case header(Req,'Content-Length') of
      [] -> {done, Req#http{entity=Data}};
      ContentLength -> {Req, Data, fun(Req1, Data1) -> 
                      assemble_body(Req1, Data1, [], string:to_integer(ContentLength))
                                   end}
    end.

assemble_body(Req, _Data, Buffered, 0) -> {done, Req#http{entity=lists:reverse(Buffered)}};
assemble_body(Req, [Char|Data], Buffered, Len) -> assemble_body(Req, Data, [Char|Buffered], Len-1);
assemble_body(_Req, [], Buffered, _Len) -> {more, Buffered}.


%Helpers
token(Data, Delim) -> token([], Data, Delim).
line(Data) -> line([], Data).

token(Token, [Delim|Input], Delim) -> {token, lists:reverse(Token), Input};
token(Token, [Char|Input], Delim) -> token([Char|Token], Input, Delim);
token(Token, [], _Delim) -> {more, Token}.

line(Token, [13, 10|Input]) -> {token, lists:reverse(Token), Input};
line(Token, [13|Input]) -> {token, lists:reverse(Token), Input};
line(Token, [10|Input]) -> {token, lists:reverse(Token), Input};
line(Token, [Char|Input]) -> line([Char|Token], Input);
line(Token, []) -> {more, Token, []}.





