%% Author: jens
%% Created: 1 mar 2013
%% Description: TODO: Add description to hty_status_resource
-module(hty_status_resource).
-record(hty_status_resource, {content, statusmap}).

-export([handle/2, mount/1, new/2]).

%%
%% API Functions
%%
mount(Fspath) ->
  {ok, Resources} = hty_mounter:walk(Fspath, "resource"),
  {Contents, Statusmap} = lists:foldl(fun(Resource, {C, S}) ->
    case is_tuple(Resource) of
      true ->
        case element(1, Resource) of
          hty_catch_resource ->
            {C, [Resource|S]};
          _ ->
            {[Resource|C], S}
        end;
      false ->
        {[Resource|C], S}
    end end, {[], []}, Resources),
  Content = hty_union_resource:new(Contents),
  {ok, hty_status_resource:new(Content, Statusmap)}.

new(Content, Statusmap) ->
  #hty_status_resource{content=Content,statusmap=Statusmap}.

handle(Htx, This) ->
  Htx1 = Htx:dispatch([This#hty_status_resource.content]),
  {Status, _} = Htx1:status(),
  io:format("Status ~p in ~p~n", [Status, This#hty_status_resource.statusmap]),
  case lists:keyfind(Status, 1, This#hty_status_resource.statusmap) of
    false ->
      Htx1;
    {_, Resource} ->
      Htx1:dispatch([Resource])
  end.
