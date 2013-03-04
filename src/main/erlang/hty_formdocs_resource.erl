%% Author: jens
-module(hty_formdocs_resource, [Fspath]).

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
		[] -> 
			case Htx:method() of
				'GET' ->							
					dirlist(Htx);
				'POST' ->
					Htx:method_not_allowed(['GET'])
			end;
		Segments ->
			Fspath1 = Fspath:subpath(Segments),
			Filepath = Fspath1:filepath(),
			case Htx:method() of
				'GET' ->							
					Htx:sendfile(Filepath);
				'POST' ->
					Htx:recvfile([fun hty_formtree_spaf:parse/2,
												fun hty_xml_spaf:format/2], Filepath)
			end
	end.
%%
%% Local Functions
%%

dirlist(Htx) ->
	Htx1 = Htx:rsp_header("Content-Type", "application/xml"),
	Htx1:echo(hty_xml:format({"dir", lists:map(fun(Fsp) ->
		{"file", [Fsp:basename()]}
	end, Fspath:list())})).