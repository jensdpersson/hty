%% Author: jens
%% Created: 31 dec 2012
%% Description: TODO: Add description to hty_union_resource
-module(hty_union_resource).
-record(hty_union_resource, {subs}).

%%
%% Exported Functions
%%
-export([handle/2, new/1]).

%%
%% API Functions
%%
new(Subs) ->
    #hty_union_resource{subs=Subs}.

handle(Htx, This) ->
    Htx:dispatch(This#hty_union_resource.subs).

