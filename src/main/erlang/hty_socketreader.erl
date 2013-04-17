%% Author: jens
%% Created: 8 apr 2013
%% Description: TODO: Add description to hty_socketreader
-module(hty_socketreader).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([recv/1]).

%%
%% API Functions
%%
-type socketreader() :: any().
-spec recv(integer()) -> timeout | 
								{data, Data::binary(), Unread::integer(), socketreader()} |
								{done, eos|limit} | {error, any()}.
recv(LeftToRead) -> 
	io:format("recv ~p more bytes~n", [LeftToRead]),
	case LeftToRead of
		Num when Num =< 0 ->
			{done, limit};
		_ ->
			receive
				{tcp, Socket, Data} ->
					inet:setopts(Socket, [{active, once}]),
					{data, Data, hty_socketreader};
				{tcp_closed, _} -> 
					{done, eos}
			end
	end.

%%
%% Local Functions
%%

