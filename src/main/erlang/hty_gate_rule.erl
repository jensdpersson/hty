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
	hty_util:std_rule_match("gate", hty_gate_resource, Fspath, Rules).


%%
%% Local Functions
%%

