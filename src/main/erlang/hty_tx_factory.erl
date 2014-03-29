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
-export([new/0, get/1, listdir/0]).

%%
%% API Functions
%%
new() -> 
	hty_tx:new(#tx{}).

get(Uri) ->
	{Segs, Query} = hty_uri:parse_path(Uri),
	(((new()):path(hty_uri:pathzipper(Segs))):queryparams(Query)):method('GET').

listdir() ->
	((new()):method('GET')):req_header('Accept', <<"text/uri-list">>).

%%
%% Local Functions
%%

