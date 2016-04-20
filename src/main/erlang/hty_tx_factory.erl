%% Author: jens
%% Created: 16 mar 2013
%% Description: TODO: Add description to hty_tx_factory
-module(hty_tx_factory).

-export([get/1, listdir/0, post/3]).

get(Uri) ->
  {Segs, Query} = hty_uri:parse_path(Uri),
  Htx = hty_tx:new(),
  Htx1 = Htx:path(hty_uri:pathzipper(Segs)),
  Htx2 = Htx1:queryparams(Query),
	Htx2:method('GET').

listdir() ->
	((hty_tx:new()):method('GET')):req_header('Accept', <<"text/uri-list">>).

post(Uri, Body, Mime) ->
	Htx = hty_tx_factory:get(Uri),
	Htx1 = Htx:method('POST'),
	Htx2 = Htx1:echo(Body),
	Htx3 = Htx2:req_header('Content-Type', Mime),
	Htx3.

%%
%% Local Functions
%%
