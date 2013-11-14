%% Author: jens
%% Created: 9 feb 2013
-module(hty_collection_rule).

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
	hty_util:std_rule_match("collection", hty_collection_resource, Fspath, Rules).


%%
%% Local Functions
%%

