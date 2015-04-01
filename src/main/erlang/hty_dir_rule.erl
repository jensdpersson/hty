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
-export([match/1]).

%%
%% API Functions
%%
match(Walker) ->
	hty_util:std_rule_match("dir", hty_dir_resource, Walker).


%%
%% Local Functions
%%
