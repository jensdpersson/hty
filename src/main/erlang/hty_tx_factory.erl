%% Author: jens
%% Created: 16 mar 2013
%% Description: TODO: Add description to hty_tx_factory
-module(hty_tx_factory).

%%
%% Include files
%%

-include("hty_tx.hrl").

%%
%% Exported Functions
%%
-export([new/0]).

%%
%% API Functions
%%
new() -> 
	hty_tx:new(#tx{}).


%%
%% Local Functions
%%

