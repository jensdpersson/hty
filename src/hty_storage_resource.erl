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
  Fspath = hty_storage:invoke_tofs(hty_tx:path_below(Htx), Storage),
  case hty_fspath:isdir(Fspath) of
    true ->
      case hty_tx:method(Htx) of
        'GET' ->
          hty_listing:list(Htx, Fspath);
        _ ->
          hty_tx:method_not_allowed(['GET'], Htx)
      end;
    false ->
      Exists = hty_fspath:exists(Fspath),
      case hty_tx:method(Htx) of
        'GET' ->
          case Exists of
            true ->
              hty_fspath:send(Htx, Fspath);
            false ->
              hty_tx:not_found(Htx)
          end;
        'PUT' ->
          Htx1 = hty_fspath:recv([], Htx, Fspath),
          case hty_tx:status(Htx1) of
            {200, _} ->
              case Exists of
                true ->
                  Htx1;
                false ->
                  hty_tx:created(Htx1)
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
