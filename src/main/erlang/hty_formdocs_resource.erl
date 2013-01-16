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
	
	case Htx:path_below() of
		[Segment] -> 
			case Htx:method() of
				'GET' ->							
					dirlist(Htx);
				'POST' ->
					Htx:method_not_allowed(['GET'])
			end;
		[Segment|Segments] ->
			Fspath1 = Fspath:subpath(Segments),
			Filepath = Fspath1:filepath(),
			case Htx:method() of
				'GET' ->							
					Htx:sendfile(Filepath);
				'POST' ->
					Htx:recvfile(
					  [fun hty_formtree_spaf:parse/1, 
					   fun hty_xml:format/1], Filepath)
			end;
		_ -> 
			Htx:not_found()
	end.
%%
%% Local Functions
%%

dirlist(Htx) ->
	Htx1 = Htx:rsp_header("Content-Type", "application/xml"),
	Htx1:echo(hty_xml:format({"dir", lists:map(fun(Fsp) ->
		{"file", [Fsp:basename()]}
	end, Fspath:list())})).