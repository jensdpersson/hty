%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_public_resource
-module(hty_sendfile_resource, [Mime, Fspath]).

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
	    Htx1 = Htx:rsp_header('Content-Type', Mime),
	    %Basename = Fspath:basename(),
	    %io:format("Basename(~p) =? PathBelow(~p)~n",[Basename, Htx:path_below()]),
	    %case Htx:path_below() of
		%[Basename] ->
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
