%% Author: jens
%% Created: 22 jan 2013
%% Description: TODO: Add description to dir_per_user_realm
-module(name_eq_pass_realm).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([auth/2,name/0]).

%%
%% API Functions
%%
auth(NameEqPass, NameEqPass) -> {ok, {NameEqPass, ["admin"]}};
auth(_, _) -> no.

name() -> "defaultrealm".
%%
%% Local Functions
%%

