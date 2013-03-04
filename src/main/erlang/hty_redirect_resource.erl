%% Author: jens
%% Created: 17 feb 2013
%% Description: TODO: Add description to hty_redirect_resource
-module(hty_redirect_resource, [UriPattern]).

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
	Query = Htx:queryparams(),
	case hty_slots:fill(UriPattern, Query) of
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

