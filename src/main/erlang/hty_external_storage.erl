%% Author: jens
%% Created: 17 apr 2013
-module(hty_external_storage).

%%
%% Include files
%%
-record(hty_external_storage, {fspath}).

%%
%% Exported Functions
%%
-export([tofs/2, mount/1, new/2]).

mount(Fspath) ->
	case Fspath:isdir() of
		false ->
			case Fspath:load() of
				{ok, Path} ->
					{ok, new(chomp(binary_to_list(Path)), Fspath)};
				{error, Error} ->
					{error, Error}
			end;
		true ->
			{error, "hty_external_storage needs to be a file, not a folder"}
	end.

chomp(Str) ->
	[_|Str1] = lists:reverse(Str),
	lists:reverse(Str1).

new(Path, Fspath) ->
	Fs = Fspath:fs(),
	Fspath1 = hty_fspath:new(Path, Fs),
	#hty_external_storage{fspath=Fspath1}.

tofs(Uripath, This) ->
	Fspath = This#hty_external_storage.fspath,
  Fspath:subpath(Uripath).
