-module(hty_ctl).

-export([start/1, stop/0]).

-spec start(atom()) -> ok | {error, string()}.
start(Path)
