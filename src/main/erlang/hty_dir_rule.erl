%% Author: jens
%% Created: 9 feb 2013
%% Description: TODO: Add description to hty_dir_rule
-module(hty_dir_rule).

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
	hty_util:std_rule_match("dir", hty_dir_resource, Fspath, Rules).


%%
%% Local Functions
%%
