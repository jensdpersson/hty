-module(hty_progress).
-export([start/0,query/2,report/2]).

-record(hty_progress, {index, completed=0, total=0, message=""}).

start() ->
    Pid = spawn(fun() -> loop(0, []) end),
    Pid.

loop(Index, Sofar) -> 
    receive 
        {report, Progress} -> 
            loop(Index+1, [{Progress#hty_progress{index=Index}}|Sofar]);
        {query, Offset, ReplyTo} ->
            Response = case Offset < 0 of
                true ->
                    lists:sublist(Sofar, Offset*(-1));
                false ->
                    case Offset == 0 of
                        true ->
                            Sofar;
                        false ->
                            lists:takewhile(fun(Progress) ->
                                Progress#hty_progress.index > Offset
                            end)
                    end
            end,
            ReplyTo ! {ok, Response},
            loop(Index, Sofar)
    end.
    
report(Progress, Pid) ->
    Pid ! {report, Progress}.
    
query(Offset, Pid) ->
    Pid ! {query, Offset, self()}.