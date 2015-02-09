%% Author: jens
%% Created: 16 feb 2013
%% Description: TODO: Add description to hty_redirect_rule
-module(hty_redirect_rule).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([match/2]).

%%
%% API Functions
%%

match(Fspath, _Rules) ->
	case Fspath:ext() of
		"redirect" ->
			case create(Fspath) of
				{no, Error} ->
					io:format("Parse of redirect file failed, ~p~n", [Error]),
					{block, Error};
				Resource ->
					{claim, {resource, Resource}}
			end;
		_ ->
			next
	end.

%%
%% Local Functions
%%

create(Fs) ->
    case Fs:load() of
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
