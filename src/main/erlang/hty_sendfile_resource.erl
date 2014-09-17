%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_public_resource
-module(hty_sendfile_resource).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/2]).
-record(hty_sendfile_resource, {mime, fspath}).
%%
%% API Functions
%%
new(Mime, Fspath) ->
    #hty_sendfile_resource{mime=Mime, fspath=Fspath}.

handle(Htx, This) ->
    case Htx:method() of
	'GET' ->
	    Htx1 = Htx:rsp_header('Content-Type', This#hty_sendfile_resource.mime),
	    %Basename = Fspath:basename(),
	    %io:format("Basename(~p) =? PathBelow(~p)~n",[Basename, Htx:path_below()]),
	    %case Htx:path_below() of
		%[Basename] ->
	    Fspath = This#hty_sendfile_resource.fspath,
	    Htx1:sendfile(Fspath:filepath());
		%_ ->
		%    Htx:not_found()
	    %end;
	_Method -> 
	    Htx:method_not_allowed(['GET']) %parametern är för att skriva en korrekt Allow-header
    end.

%%
%% Local Functions
%%
