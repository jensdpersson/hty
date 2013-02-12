%% Author: jens
%% Created: 10 feb 2013
%% Description: TODO: Add description to hty_shift_rule
-module(hty_shift_rule).

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
	hty_util:std_rule_match("shift", hty_shift_resource, Fspath, Rules).


%%
%% Local Functions
%%

