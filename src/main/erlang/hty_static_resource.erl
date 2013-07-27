%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_public_resource
-module(hty_static_resource, [Fspath]).

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
handle(Htx) ->
    case Htx:method() of
	'GET' ->
						%Htx1 = Htx:rsp_header('Content-Type', "text/html"),
	    case Fspath:subpath(Htx:path_below()) of
		ascension_denied ->
		    Htx:not_found();
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
				[Welcome|_] -> 
				    Htx2 = Htx:rsp_header('Content-Type', Htx:mimemap(Welcome:ext())),
				    Htx3 = Htx2:sendfile(Welcome:filepath()),
				    Htx3:ok();
				[] -> 
				    Htx:not_found()
			    end;
			false ->
			    case Fspath1:exists() of
				true ->
				    Htx2 = Htx:rsp_header('Content-Type', Htx:mimemap(Fspath1:ext())),
				    Htx3 = Htx2:sendfile(Fspath1:filepath()),
				    Htx3:ok();
				false ->
				    Htx:not_found()
			    end
		    end
	    end;
	_Method -> Htx:method_not_allowed(['GET']) %parametern är för att skriva en korrekt Allow-header
    end.

%%
%% Local Functions
%%
