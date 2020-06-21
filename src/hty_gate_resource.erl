%% Author: jens
%% Created: 7 feb 2013
%% Description: TODO: Add description to hty_gate_resource
-module(hty_gate_resource).
-record(hty_gate_resource, {lookup, subs}).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/2, mount/2]).

%%
%% API Functions
%%
mount(Fspath, Mc) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["gate"|Rest] ->
      case hty_mounter:walk(Fspath, "resource", Mc) of
        {ok, Subs} ->
          case Rest of
            [] ->
              Lookup = fun(Htx) ->
                [Seg|_Segs] = hty_tx:path_below(Htx),
                list_to_binary(Seg)
              end,
              {ok, hty_gate_resource:new(Lookup, Subs)};
            [Role|_] ->
              Lookup = fun(_) -> Role end,
              {ok, hty_gate_resource:new(Lookup, Subs)}
          end;
        {error, _} = Error ->
          Error
      end
  end.

new(Lookup, Subs) ->
  #hty_gate_resource{lookup=Lookup, subs=Subs}.

handle(Htx, This) ->
  Lookup = This#hty_gate_resource.lookup,
  Subs = This#hty_gate_resource.subs,
  io:format("Principal = ~p~n", [hty_tx:principal(Htx)]),
  {_Nick, Roles} = hty_tx:principal(Htx),
  Role = Lookup(Htx),
  io:format("member? ~p ~p~n", [Role, Roles]),
  case lists:member(Role, Roles) of
    true ->
      hty_tx:dispatch(Subs, Htx);
    false ->
      hty_tx:forbidden(Htx)
  end.
