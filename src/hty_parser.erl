%
% @doc HTTP parser
%
-module(hty_parser).

-export([parse/1, respond/2]).

pack_status({Code, Message}) when is_integer(Code) -> integer_to_list(Code) ++ " " ++ Message.

pack_header(HeaderName, HeaderValue) ->
  to_list(HeaderName) ++ ": " ++ to_list(HeaderValue).

to_list(Any) ->
  case is_atom(Any) of
    true ->
      atom_to_list(Any);
    false ->
      case is_binary(Any) of
        true ->
          binary_to_list(Any);
        false ->
          Any
      end
  end.

respond(Socket, Htx) ->
    send_line(Socket, "HTTP/1.1 " ++ pack_status(hty_tx:status(Htx))),
    send_line(Socket, "Server: haughty 0.4"),
    lists:foreach(fun({HName, HValue}) ->
      send_line(Socket, pack_header(HName, HValue))
    end, hty_tx:rsp_headers(Htx)),
    send_line(Socket, ""),
    lists:foreach(fun(Out) ->
      case Out of
        {file, Filepath} ->
          io:format("sendfile('~p')~n",[Filepath]),
          case file:sendfile(Filepath, Socket) of
            {ok, _BytesSent} ->
              io:format("Sendfile ok~n");
            {error, Reason} ->
              io:format("Sendfile fails with ~p~n", [Reason])
          end;
        {data, IOList} ->
          io:format("data('~p')~n", [IOList]),
          gen_tcp:send(Socket, IOList)
      end
    end, lists:reverse(hty_tx:outs(Htx))).

send_line(Socket, Line) -> gen_tcp:send(Socket,  Line ++ "\r\n").

set_peer(Htx, Socket) ->
  case inet:peername(Socket) of
    {ok, {Address, Port}} ->
      case Address of
        {_,_,_,_} ->
          hty_tx:peer({ipv4, Address, Port}, Htx);
        {_,_,_,_,_,_,_,_} ->
          hty_tx:peer({ipv6, Address, Port}, Htx);
        _ ->
          hty_tx:peer({other, Address, Port}, Htx)
      end;
    {error, _}	->
      Htx
  end.

parse(Socket) ->
    Htx = hty_tx:new(),
    Htx0 = set_peer(Htx, Socket),
    parse_loop(Htx0, <<>>, fun method_parser/2, Socket).

parse_loop(Htx, Unparsed, Parser, Socket) ->
    case Parser(Htx, Unparsed) of
  {Htx1, Unparsed1, Parser1} ->
      parse_loop(Htx1, Unparsed1, Parser1, Socket);
  more ->
      recv(Htx, Unparsed, Parser, Socket);
  {more, []} ->
      recv(Htx, Unparsed, Parser, Socket);
  {done, Htx1} ->
      hty_tx:socketreader(hty_socketreader, Htx1);
  {error, Error} ->
      {error, Error}
    end.

recv(Htx, Unparsed, Parser, Socket) ->
    receive
       {tcp, Socket1, Data} ->
           inet:setopts(Socket, [{active, once}]),
           Unparsed1 = <<Unparsed/binary, Data/binary>>,
           parse_loop(Htx, Unparsed1, Parser, Socket1);
       {tcp_closed, _} -> Htx
    end.

method_parser(Htx, Data) ->
    case token(Data, 32) of
        {token, Method, Rest} ->
          {hty_tx:method(canonical_method(Method), Htx),Rest,fun path_parser/2};
        {more, _Token} -> more
    end.

canonical_method(<<"GET">>) -> 'GET';
canonical_method(<<"POST">>) -> 'POST';
canonical_method(<<"PUT">>) -> 'PUT';
canonical_method(<<"DELETE">>) -> 'DELETE';
canonical_method(<<"OPTIONS">>) -> 'OPTIONS';
canonical_method(<<"HEAD">>) -> 'HEAD';
canonical_method(Method) when is_binary(Method) ->
  list_to_atom(binary_to_list(Method)).

path_parser(Htx, Data) ->
    case token(Data, 32) of
  {token, Path, Rest} ->
      {Segments, Query} = hty_uri:parse_path(Path),
      Pathzipper = hty_uri:pathzipper(Segments),
      Htx1 = hty_tx:path(Pathzipper, Htx),
      {hty_tx:queryparams(Query, Htx1),
       Rest, fun protocol_parser/2};
  {more, _Token} ->
      more
    end.

protocol_parser(Req, Data) ->
    case line(Data) of
        {token, Protocol, Rest} ->
      {hty_tx:protocol(Protocol, Req),Rest,fun header_parser/2};
        {more, _Token} ->
      more
    end.


header_parser(Htx, <<13,10,Data/binary>>) -> {Htx, Data, fun body_parser/2};
header_parser(Htx, <<13, Data/binary>>) -> {Htx, Data, fun body_parser/2};
header_parser(Htx, <<10, Data/binary>>) -> {Htx, Data, fun body_parser/2};
header_parser(Htx, Data) ->
    case token(Data, $:) of
        {token, Name, Data1} ->
            case line(Data1) of
              {token, Value, Data2} ->
                {
                 hty_tx:req_header(string:to_lower(binary_to_list(Name)),
                                hty_util:ltrim(Value), Htx),
                 Data2,
                 fun header_parser/2};
               {more, _Token} -> more
            end;
        {more, _Token} -> more
    end.

body_parser(Htx, Data) ->
  Leng = get_content_length(Htx),
  Htx1 = hty_tx:unread(Leng, Htx),
  {done, hty_tx:buffer(Data, Htx1)}.

get_content_length(Htx) ->
  case hty_tx:req_header('Content-Length', Htx) of
    [] -> -1;
    [ContentLength] ->
        {Len, []} = string:to_integer(binary_to_list(ContentLength)),
        io:format("Conlen [~p]~n",[ContentLength]),
        Len
        %{Htx,
         %Data,
         %fun (Htx1, Data1) ->
        %			assemble_body(Htx1, Data1, Len)
        % end
        %}
  end.

%assemble_body(Htx, Data, Len) ->
%	case Len =< 0 of
%		true ->
%			{done, Htx:buffer(Data)};
%		false ->
%			{
%			 Htx:buffer(Data),
%			 <<>>,
%			 fun (Htx1, Data1) ->
%						io:format("Len [~p]~n", [Len]),
%						assemble_body(Htx1, Data1, Len - size(Data))
%			end}
%	end.

%Helpers
token(Data, Delim) ->
  {Token, Rest} = hty_scan:until(Data, Delim),
  case Rest of
    <<Delim:8/integer, Rest1/binary>> ->
      {token, Token, Rest1};
    <<>> ->
      {more, Token}
  end.

line(Data) ->
  {Line, Rest} = hty_scan:until_either(Data, 13, 10),
  case Rest of
    <<13, 10, R2/binary>> ->
      {token, Line, R2};
    <<13, R2/binary>> ->
      {token, Line, R2};
    <<10, R2/binary>> ->
      {token, Line, R2};
    <<>> ->
      {more, Line}
  end.


%token(Token, [Delim|Input], Delim) -> {token, lists:reverse(Token), Input};
%token(Token, [Char|Input], Delim) -> token([Char|Token], Input, Delim);
%token(Token, [], _Delim) -> {more, Token}.

%line(Token, [13, 10|Input]) -> {token, lists:reverse(Token), Input};
%line(Token, [13|Input]) -> {token, lists:reverse(Token), Input};
%line(Token, [10|Input]) -> {token, lists:reverse(Token), Input};
%line(Token, [Char|Input]) -> line([Char|Token], Input);
%line(Token, []) -> {more, Token, []}.
