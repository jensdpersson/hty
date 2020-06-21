%% Author: jens
%% Created: 17 apr 2013
-module(hty_flat_storage).

%%
%% Include files
%%
-record(hty_flat_storage, {fspath}).

%%
%% Exported Functions
%%
-export([tofs/2, mount/2, new/1]).

mount(Fspath, _Mc) ->
	{ok, new(Fspath)}.

new(Fspath) ->
	#hty_flat_storage{fspath=Fspath}.

tofs(Uripath, This) ->
	Fspath = This#hty_flat_storage.fspath,
  hty_fspath:subpath(Uripath, Fspath).
