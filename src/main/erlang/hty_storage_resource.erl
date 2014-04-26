%% Author: jens
-module(hty_storage_resource, [Tofs]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/1, exists/1]).

%%
%% API Functions
%%

handle(Htx) ->
	Fspath = Tofs(Htx:path_below()),
	case Fspath:is_dir() of
		true ->
			case Htx:method() of
				'GET' ->							 
					hty_listing:list(Htx, Fspath);
				_ ->
					Htx:method_not_allowed(['GET'])
			end;
		false ->
			Filepath = Fspath:filepath(),
			Exists = Fspath:exists(),
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

exists(PathBelow) ->
	Htx = ((hty_tx_factory:new()):method('HEAD')):path_below(PathBelow),
	Htx1 = Htx:dispatch(this()),
	case Htx1:status() of
		{200, _} ->
			true;
		{204, _} ->
			true;
		_ -> false
	end.

get(PathBelow) ->
	Htx = ((hty_tx_factory:new()):method('GET')):path_below(PathBelow),
	Htx:dispatch(this()).

put(PathBelow, Entity, ContentType) ->
	Htx = ((hty_tx_factory:new()):method('PUT')):path_below(PathBelow),
	Htx2 = (Htx:buffer(Entity)):req_header('Content-Type', ContentType),
	Htx2:dispatch(this()).

%%
%% Local Functions
%%
this() ->
	hty_storage_resource:new(Tofs).
