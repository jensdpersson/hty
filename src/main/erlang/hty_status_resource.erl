%% Author: jens
%% Created: 1 mar 2013
%% Description: TODO: Add description to hty_status_resource
-module(hty_status_resource, [Fspath, Content]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/1]).

%%
%% API Functions
%%
handle(Htx) ->
	Htx1 = Htx:dispatch([Content]),
	{Status, _} = Htx1:status(),
	IsStatusFileFor = fun(Fs) ->
												 Prefix = Fs:prefix(),
												 case string:tokens(Prefix, "_") of
													 ["status", Code] ->
														 case list_to_integer(Code) of
															 Status ->
																 true;
															 _ ->
																 false
														 end;
													 _ ->
												 	false
												 end
										end,
	Statusfiles = Fspath:list(IsStatusFileFor),
	case Statusfiles of
		[Statusfile] ->
			Htx1:sendfile(Statusfile:filepath());
		[] -> Htx1
	end.


%%
%% Local Functions
%%

