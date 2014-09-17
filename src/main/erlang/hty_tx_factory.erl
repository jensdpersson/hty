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
-export([get/1, listdir/0]).

%%
%% API Functions
%%

get(Uri) ->
	{Segs, Query} = hty_uri:parse_path(Uri),
	(((hty_tx:new()):path(hty_uri:pathzipper(Segs))):queryparams(Query)):method('GET').

listdir() ->
	((hty_tx:new()):method('GET')):req_header('Accept', <<"text/uri-list">>).

%%
%% Local Functions
%%

