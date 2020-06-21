%% Author: jens
%% Created: 16 mar 2013
%% Description: TODO: Add description to hty_tx_factory
-module(hty_tx_factory).

-export([get/1, listdir/0, post/3]).

get(Uri) ->
  {Segs, Query} = hty_uri:parse_path(Uri),
  Htx = hty_tx:new(),
  Htx1 = hty_tx:path(hty_uri:pathzipper(Segs), Htx),
  Htx2 = hty_tx:queryparams(Query, Htx1),
	hty_tx:method('GET', Htx2).

listdir() ->
    Htx = hty_tx:new(),
    hty_tx:with([
        {method, 'GET'},
        {req_header, 'Accept', <<"text/uri-list">>}
    ], Htx).

post(Uri, Body, Mime) ->
	Htx = hty_tx_factory:get(Uri),
	Htx1 = hty_tx:method('POST', Htx),
	Htx2 = hty_tx:echo(Body, Htx1),
	Htx3 = hty_tx:req_header('Content-Type', Mime, Htx2),
	Htx3.

%%
%% Local Functions
%%
