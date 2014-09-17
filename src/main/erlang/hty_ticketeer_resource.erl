-module(hty_ticketeer_resource).

-export([handle/2, new/1]).

-record(hty_ticketeer_resource, {logfolder}).

new(Logfolder) ->
    #hty_ticketeer_resource{logfolder=Logfolder}.

handle(Htx, This) ->
    Logfolder = This#hty_ticketeer_resource.logfolder,
    io:format("Enter Ticketeer~n"),
    File = Logfolder:subpath([hty_log:today() ++ ".log"]),
    {ok, Fd} = file:open(File:filepath(), [append, binary]),
    Gol = Htx:log(),
    Log = lists:reverse(Gol),
    io:format("Ticketeer Log, !--~p--!~n", [Log]),
    case lists:foldl(fun(Entry, Acc) -> 
			     case Acc of 
				 ok ->
				     case Entry of
					 {Context, T, Evt, Data} ->
					     Msg = [T, $|, Context, $|, Evt, $|, Data, 10],
					     io:format("Logging <~p>~n", [Msg]),
					     file:write(Fd, Msg);
					 _ ->
					     io:format("Strange Log Entry [~p]~n", [Entry])
				     end;
				 Error ->
				     Error
			     end
		     end,
		     ok,
		     Log) of
	ok ->
	    io:format("Exit ticketeer");
	{error, Reason} ->
	    io:format("Failed writing log, ~p~n", [Reason])
    end,
    Htx.
    
    
