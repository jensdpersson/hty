%% Author: jens
%% Created: 17 feb 2013
%% Description: TODO: Add description to hty_redirect_resource
-module(hty_redirect_resource).
-record(hty_redirect_resource, {uripattern}).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/1]).
-export([mount/1]).
%%
mount(Fspath) ->
	case hty_fspath:ext(Fspath) of
		"redirect" ->
			case create(Fspath) of
				{no, Error} ->
					io:format("Parse of redirect file failed, ~p~n", [Error]),
					{block, Error};
				Resource ->
					{ok, Resource}
			end;
		_ ->
			next
	end.

%%
%% Local Functions
%%

create(Fspath) ->
    case hty_fspath:load(Fspath) of
	{ok, Content} ->
	    case hty_slots:parse(Content) of
		{no, Error} ->
		    {no, Error};
		Slots ->
		    hty_redirect_resource:new(Slots)
	    end;
	{error, Error} ->
	    {no, Error}
    end.

%% API Functions
%%

new(UriPattern) ->
    #hty_redirect_resource{uripattern=UriPattern}.

handle(Htx, This) ->
	Query = hty_tx:queryparams(Htx),
	case hty_slots:fill(This#hty_redirect_resource.uripattern, Query) of
		{no, {"missingkey", Key}} ->
			Htx1 = hty_tx:bad_request(Htx),
			hty_tx:echo("missing parameter " ++ Key, Htx1);
		{no, _Error} ->
			hty_tx:server_error(Htx);
		URL ->
			hty_tx:see_other(URL, Htx)
	end.

%%
%% Local Functions
%%
