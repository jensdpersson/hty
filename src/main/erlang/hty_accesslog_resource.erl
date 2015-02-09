-module(hty_accesslog_resource).

-export([new/3, handle/2]).

-record(hty_accesslog_resource, {format, folder, subs}).

new(Format, Folder, Subs) ->
  #hty_accesslog_resource{format=Format, folder=Folder, subs=Subs}.

handle(Htx, This) ->
    {_Mod, _Format, Logfolder, Subs} = This,
    io:format("Enter accesslog"),
    Method = atom_to_list(Htx:method()),
    Path = hty_uri:pack(Htx:path()),
    T0 = hty_log:tstamp(),
    Htx1 = Htx:dispatch(Subs),
    {StatusCode, StatusText} = Htx1:status(),
    T1 = hty_log:tstamp(),
    File = Logfolder:subpath([hty_log:today() ++ ".log"]),
    Msg = [T0, $|, Method, $|, Path, $|, T1, $|, integer_to_list(StatusCode), " ", StatusText, 10],
    io:format("Access logging ~p~n", [Msg]),
    case File:append(Msg) of
	     ok ->
	        io:format("Exit accesslog");
	     {error, Reason} ->
	        io:format("Failed writing log, ~p~n", [Reason])
    end,
    Htx1.
