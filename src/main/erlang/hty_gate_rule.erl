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
-export([match/1]).

%%
%% API Functions
%%

match(Walker) ->
  Fspath = Walker:fspath(),
    case lists:reverse(Fspath:parts()) of
	["gate"|Rest] ->
	    Subs = Walker:subs(),
	    case Rest of
		[] ->
		    Lookup = fun segment_lookup/1,
        {claim, {resource, hty_gate_resource:new(Lookup, Subs)}};
		[Role|_] ->
		    %{block, "unsupported"}
        Lookup = fun(_) -> Role end,
        {claim, {resource, hty_gate_resource:new(Lookup, Subs)}}
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
