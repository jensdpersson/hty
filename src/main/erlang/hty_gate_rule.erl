%% Author: jens
%% Created: 7 feb 2013
%% Description: TODO: Add description to hty_gate_rule
-module(hty_gate_rule).

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
	case lists:reverse(Fspath:parts()) of
		["gate", Role] ->
			Subs = hty_util:subs(Fspath, Rules),
			case Role of
				"$segment" -> 
					Lookup = fun segment_lookup/1,
					{claim, {resource, hty_gate_resource:new(Lookup, Subs)}};
				_ ->
					{block, "unsupported"}
			end;
		_ ->
			next
	end.

%%
%% Local Functions
%%
segment_lookup(Htx) ->
	[Seg|_Segs] = Htx:path_below(),
	list_to_binary(Seg).
