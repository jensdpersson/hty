%% Author: jens
-module(hty_storage_resource, [Fspath, Tofs]).

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
	PathBelow = Tofs(Htx:path_below()),
	Index = hty_listing_resource:new("", Fspath),
	case PathBelow of
		[] ->
			case Htx:method() of
				'GET' ->							 
					Htx:dispatch([Index]);
				_ ->
					Htx:method_not_allowed(['GET'])
			end;
		Segments ->
			Fspath1 = Fspath:subpath(Segments),
			Filepath = Fspath1:filepath(),
			Exists = Fspath1:exists(),
			case Htx:method() of
				'GET' ->
					case Exists of
						true ->
							Htx:sendfile(Filepath);
						false ->
							Htx:not_found()
					end;
				'PUT' ->
					Htx1 = Htx:recvfile([], Filepath),
					case Htx1:status() of 
						{200, _} ->
							case Exists of
								true ->
									Htx1;
								false ->
									Htx1:created()
							end
					end
			end
	end.
%%
%% Local Functions
%%

