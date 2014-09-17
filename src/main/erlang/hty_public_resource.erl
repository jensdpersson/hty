%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_public_resource
-module(hty_public_resource).
-record(hty_public_resource, {fspath}).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/1]).

%%
%% API Functions
%%

new(Fspath) ->
    #hty_public_resource{fspath=Fspath}.

handle(Htx, This) ->
    Fspath = This#hty_public_resource.fspath,
    case Htx:method() of
	'GET' ->
	    Htx1 = Htx:rsp_header('Content-Type', "text/html"),
	    case Fspath:subpath(Htx:path_below()) of
		notfound ->
		    Htx1:not_found();
		Fspath1 ->
		    case Fspath1:isdir() of
			true ->
			    F = fun(W) ->
					Fspath2 = Fspath1:subpath([W]),
					io:format("Does ~p exist?~n", [Fspath2:filepath()]),
					case Fspath2:exists() of
					    true -> [Fspath2];
					    false -> []
					end
				end,
			    L = ["index.html", "index.xml"],
			    case lists:flatmap(F, L) of
				[Welcome|_] -> Htx1:sendfile(Welcome:filepath());
				[] -> Htx1:not_found()
			    end;
			false ->
			    Htx1:sendfile(Fspath1:filepath())
		    end
	    end;
	_Method -> Htx:method_not_allowed(['GET'])
    end.

%%
%% Local Functions
%%
