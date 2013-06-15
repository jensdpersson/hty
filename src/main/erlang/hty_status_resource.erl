%% Author: jens
%% Created: 1 mar 2013
%% Description: TODO: Add description to hty_status_resource
-module(hty_status_resource, [Content, Statusmap]).

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
    Htx1 = Htx:dispatch([Content]),
    {Status, _} = Htx1:status(),
    case lists:keyfind(Status, 1, Statusmap) of
	false ->
	    Htx1;
	{_, Resource} ->
	    Htx1:dispatch([Resource])
    end.

%%
%% Local Functions
%%

