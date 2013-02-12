%% Author: jens
%% Created: 31 dec 2012
%% Description: TODO: Add description to hty_union_resource
-module(hty_union_resource, [Subs]).

%%
%% Exported Functions
%%
-export([handle/1]).

%%
%% API Functions
%%
handle(Htx) -> Htx:dispatch(Subs).

