%% Author: jens
-module(hty_history_resource, [Fspath, DiskFormat]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/1]).

%%
%% API Functions
%%

handle(Htx0) ->
    Method = Htx0:method(),
    case Htx0:path_below() of
	[] ->
	    case Method of
		'GET' ->
			Htx1 = Htx0:bind("xslpi_choice", "list"),
		    hty_listing:list(Htx1, Fspath, [last_modified]);
		'POST' ->
		    Htx0:method_not_allowed(['GET'])
	    end;
	[Segment] ->
		Htx = Htx0:bind("xslpi_choice", "doc"),
	    Fspath1 = Fspath:subpath([Segment]),
	    Rev = Htx:matrix(<<"rev">>),
	    io:format("Processing ~p", [Segment]),
	    case Rev of
		no ->
		    io:format("~n"),
		    case tip(Fspath1) of
			nofile ->
			    case Method of
				'GET' -> 
				    Htx:not_found();
				'POST' ->
				    Fspath1:mkdir(),
				    save(Htx, "0", Fspath1)
			    end;
			notip ->
			    redirect(Htx, "0");
			noread ->
			    Htx:server_error("FailedReadingTip");
			Tip ->
			    redirect(Htx, Tip)
		    end;
		_ ->
		    io:format(";rev=~p~n", [Rev]),
		    Fspath2 = Fspath1:subpath([binary_to_list(Rev) ++ ".data"]),
		    Filepath = Fspath2:filepath(),
		    case Method of
			'GET' ->	
			    case Fspath2:exists() of
				true -> 	
                                    Htx1 = Htx:ok(),
				    Htx1:sendfile(Filepath);
				false ->
				    case tip(Fspath1) of
					nofile -> 
					    Htx:not_found();
					notip -> 
					    Htx:not_found();
					noread -> 
					    Htx:server_error("FailedReadingTip");
					Tip ->
					    redirect(Htx, Tip)
				    end
			    end;
			'POST' ->
			    %Is this tip?
			    case tip(Fspath1) of
				nofile ->
				    Htx:not_found();
				notip ->
				    Htx:not_found();
				%No tip means file is considered 'deleted'
				   % case Rev of
				%	<<"0">> ->
				%	    save(Htx, "0", Fspath1:subpath(["0.data"]));
				%	_ ->
				%	    Htx:not_found()
				 %   end;
				noread ->
				    Htx:server_error("FailedReadingTip");
				Rev ->
				    Int = list_to_integer(binary_to_list(Rev)),
				    Int1 = Int + 1,
				    Rev1 = integer_to_list(Int1),
				    save(Htx, Rev1, Fspath1);
				Num ->
				    io:format("Rev does not match tip: [~p != ~p]", [Rev, Num]),
				    conflict(Htx, Num, Rev)
			    end;
			_ ->
			    Htx:method_not_allowed(['GET', 'POST'])
				
		    end
	    end
    end.

conflict(Htx, _Tip, _Rev) ->
    Htx:conflict().

redirect(Htx, Tip) ->
    Location = hty_uri:matrix(Htx:path(), <<"rev">>, Tip),
    Htx:temporary_redirect(hty_uri:pack(Location)).

save(Htx, Tip, Fs) ->
    Spaf = spaf(Htx:req_header('Content-Type'), DiskFormat),
    Datafile = Fs:subpath([Tip ++ ".data"]),
    Htx1 = Htx:recvfile(Spaf, Datafile:filepath()),
    Tipfile = Fs:subpath(["tip"]),
    case file:write_file(Tipfile:filepath(), Tip) of
	{error, Error} ->
	    Htx1:server_error(Error);
	ok ->
	    Location = hty_uri:matrix(Htx1:path(), <<"rev">>, Tip),
	    Htx1:see_other(hty_uri:pack(Location))
    end.

tip(Fs) ->
    case Fs:exists() of
	false ->
	    nofile;
	true ->
	    Fs1 = Fs:subpath(["tip"]),
	    case Fs1:exists() of
		false -> 
		    notip;
		true ->
		    case file:read_file(Fs1:filepath()) of 
			{ok, Content} ->
			    Content;
			{error, _Error} ->
			    noread
		    end
	    end
    end.


spaf([<<"application/x-www-form-urlencoded">>], "xml") -> [fun hty_formtree_spaf:parse/2, fun hty_xml_spaf:format/2];
spaf(Mime, Diskf) ->
    io:format("Unregged mime/diskformat combo [~p/~p]~n", [Mime, Diskf]).
