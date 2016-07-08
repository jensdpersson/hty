%% Author: jens
%% Created: 9 feb 2013
%% Description: TODO: Add description to hty_dir_resource
-module(hty_dir_resource).
-record(hty_dir_resource, {segment, subs}).

-export([mount/1, handle/2, new/2]).

mount(Fspath) ->
  case lists:reverse(Fspath:parts()) of
    ["dir", Name | _ ] ->
      case hty_mounter:walk(Fspath, "resource") of
        {ok, Subs} ->
          {ok, new(Name, Subs)};
        {error, _} = E ->
          E
      end;
    _ ->
      {error, "dir resource needs a name parameter"}
  end.

new(Segment, Subs) ->
    #hty_dir_resource{segment=Segment, subs=Subs}.

handle(Htx, This) ->
  Segment = This#hty_dir_resource.segment,
  Subs = This#hty_dir_resource.subs,
  case Htx:consume(Segment) of
    no ->
      Htx:not_found();
    Htx1 ->
      Htx1:dispatch(Subs)
  end.


%%
%% Local Functions
%%
