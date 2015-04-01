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
-export([match/1]).

%%
%% API Functions
%%
match(Walker) ->
	Fspath = Walker:fspath(),
	case Fspath:prefix() of
		"index" ->
			Walker2 = Walker:rules(Walker:rules() -- [?MODULE]),
			case Walker2:match() of
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
