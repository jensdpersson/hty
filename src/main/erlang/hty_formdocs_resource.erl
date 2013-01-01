%% Author: jens
-module(hty_formdocs_resource, [Fspath, Segment]).

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
	io:format("formdocs:handle(~p)~n", [Htx:path_below()]),
	case Htx:method() of
		'POST' ->
			Htx:method_not_allowed(['GET']);
		'GET' ->			
			case Htx:path_below() of
				[Segment] -> 
					dirlist(Htx);
				_ -> 
					Htx:not_found()
			end
	end.
%%
%% Local Functions
%%

dirlist(Htx) ->
	Htx1 = Htx:rsp_header('Content-Type', "application/xml"),
	Htx1:echo(hty_xml:format({"dir", lists:map(fun(Fsp) ->
		{"fil", [Fsp:basename()]}
	end, Fspath:list())})).