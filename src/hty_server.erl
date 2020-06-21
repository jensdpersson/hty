-module(hty_server).
-export([start/1, stop/1]).

-callback start(Server::tuple()) -> ok | {no, Why::string()}.
-callback stop(Server::tuple()) -> ok | {no, Why::string()}.

start(Server) -> (element(1, Server)):start(Server).
stop(Server) -> (element(1, Server)):stop(Server).

