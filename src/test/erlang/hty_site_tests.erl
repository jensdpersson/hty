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
	Pid = hty_site:start(),
	ok = hty_site:mount(Pid, [], root),
	ok = hty_site:mount(Pid, [js], dir),
	ok = hty_site:mount(Pid, [js, 'bogus.js'], jslib),
	jslib = hty_site:lookup(Pid, [js, 'bogus.js']).

%%
%% Local Functions
%%

