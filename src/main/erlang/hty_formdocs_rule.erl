%% Author: jens
%% Created: 1 jan 2013
%% Description: TODO: Add description to hty_formdocs_rule
-module(hty_formdocs_rule).

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
	case Fspath:ext() of
		"formdocs" ->
			Index = hty_listing_resource:new("", Fspath),
			{claim, {resource, hty_formdocs_resource:new(Fspath, Index)}};
		_ ->
			next
	end.

%	case lists:reverse(Fspath:parts()) of
%		["formdocs", IndexClassName|_] ->
%			IndexClass = list_to_atom(IndexClassName),
%			Index = IndexClass:new(Fspath),
%			{claim, {resource, hty_formdocs_resource:new(Fspath, Index)}};
%		_ ->
%			next
%	end.


%%
%% Local Functions
%%
