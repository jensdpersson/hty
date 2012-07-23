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

%%
%% Test functions
%%
basic_test() ->
	Pid = hty_site:start(),
	hty_site:mount(Pid, [], root),
	hty_site:mount(Pid, [js], dir),
	hty_site:mount(Pid, [js, 'bogus.js'], jslib),
	jslib = hty_site:lookup(Pid, [js, 'bogus.js']).

%%
%% Local Functions
%%

