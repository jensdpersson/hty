%% Author: jens
-module(hty_storage_resource).
-record(hty_storage_resource, {storage}).

-export([handle/2, new/1]).%, exists/2, get/2, put/4]).
-export([mount/1]).

mount(Fspath) ->
  case hty_mounter:walk(Fspath, "storage") of
    {ok, [Storage]} ->
      {ok, #hty_storage_resource{storage=Storage}};
    {error, _Error} = E ->
      E
  end.

new(Storage) ->
  #hty_storage_resource{storage=Storage}.

handle(Htx, This) ->
  Storage = This#hty_storage_resource.storage,
  Fspath = Storage:tofs(Htx:path_below()),
  case Fspath:isdir() of
    true ->
      case Htx:method() of
        'GET' ->
          hty_listing:list(Htx, Fspath);
        _ ->
          Htx:method_not_allowed(['GET'])
      end;
    false ->
      Exists = Fspath:exists(),
      case Htx:method() of
        'GET' ->
          case Exists of
            true ->
              Fspath:send(Htx);
            false ->
              Htx:not_found()
          end;
        'PUT' ->
          Htx1 = Fspath:recv([], Htx),
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

%exists(PathBelow, This) ->
%    Htx = ((hty_tx_factory:new()):method('HEAD')):path_below(PathBelow),
%    Htx1 = Htx:dispatch(This),
%    case Htx1:status() of
%      {200, _} ->
%        true;
%      {204, _} ->
%        true;
%      _ -> false
%    end.

%get(PathBelow, This) ->
%    Htx = ((hty_tx_factory:new()):method('GET')):path_below(PathBelow),
%    Htx:dispatch(This).

%put(PathBelow, Entity, ContentType, This) ->
%    Htx = ((hty_tx_factory:new()):method('PUT')):path_below(PathBelow),
%    Htx2 = (Htx:buffer(Entity)):req_header('Content-Type', ContentType),
%    Htx2:dispatch(This).
