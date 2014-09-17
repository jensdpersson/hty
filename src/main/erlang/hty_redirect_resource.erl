%% Author: jens
%% Created: 17 feb 2013
%% Description: TODO: Add description to hty_redirect_resource
-module(hty_redirect_resource).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/1]).
-record(hty_redirect_resource, {uripattern}).
%%
%% API Functions
%%

new(UriPattern) ->
    #hty_redirect_resource{uripattern=UriPattern}.

handle(Htx, This) ->
	Query = Htx:queryparams(),
	case hty_slots:fill(This#hty_redirect_resource.uripattern, Query) of
		{no, {"missingkey", Key}} ->
			Htx1 = Htx:bad_request(),
			Htx1:echo("missing parameter " ++ Key);
		{no, _Error} ->
			Htx:server_error();
		URL ->
			Htx:see_other(URL)
	end.

%%
%% Local Functions
%%

