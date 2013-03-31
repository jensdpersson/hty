%% Author: jens
%% Created: 9 mar 2013
%% Description: TODO: Add description to hty_index_rule
-module(hty_index_rule).

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
match(Fspath, Rules) ->
	case Fspath:prefix() of
		"index" ->
			Rules1 = Rules -- [?MODULE],
			case Fspath:match(Rules1) of
				{ok, {resource, Resource}, _, _} ->
					Rv = hty_index_resource:new(Fspath, Resource),
					{claim, {resource, Rv}};
				{no, Reason, _, _} ->
					{block, Reason}
			end;
		_ -> 
			next
	end.

%%
%% Local Functions
%%

