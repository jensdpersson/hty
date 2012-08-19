%% Author: jens
%% Created: 19 jul 2012
%% Description: TODO: Add description to hty_site_tests
-module(hty_site_tests).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([basic_test/0]).

%%
%% API Functions
%%
basic_test() ->
	Site = hty_sites:create(aSiteId),
	io:format("Mounting root"),
	ok = Site:mount(Pid, [], root),
	io:format("Mounting dir"),
	ok = Site:mount(Pid, [js], dir),
	io:format("Mounting jslib"),
	ok = Site:mount(Pid, [js, 'bogus.js'], jslib),
	io:format("Lookup jslib"),
	{ok, jslib} = Site:lookup(Pid, [js, 'bogus.js']).

%%
%% Local Functions
%%

