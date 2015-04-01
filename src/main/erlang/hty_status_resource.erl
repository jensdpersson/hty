%% Author: jens
%% Created: 1 mar 2013
%% Description: TODO: Add description to hty_status_resource
-module(hty_status_resource).
-record(hty_status_resource, {content, statusmap}).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/2]).

%%
%% API Functions
%%
new(Content, Statusmap) ->
    #hty_status_resource{content=Content,statusmap=Statusmap}.

handle(Htx, This) ->
    Htx1 = Htx:dispatch([This#hty_status_resource.content]),
    {Status, _} = Htx1:status(),
    io:format("Status ~p in ~p~n", [Status, This#hty_status_resource.statusmap]),
    case lists:keyfind(Status, 1, This#hty_status_resource.statusmap) of
	false ->
	    Htx1;
	{_, Resource} ->
	    Htx1:dispatch([Resource])
    end.

%%
%% Local Functions
%%
