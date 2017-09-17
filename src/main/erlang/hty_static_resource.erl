%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_public_resource
-module(hty_static_resource).

-record(hty_static_resource, {fspath, welcome}).


%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/2, mount/1]).


mount(Fspath) ->
  Welcome = case Fspath:param("welcome") of
    no ->
      hty_indexfile_welcome:new();
    WelcomeType ->
      WelcomeModule = list_to_atom("hty_" ++ WelcomeType ++ "_welcome"),
      WelcomeModule:new()
  end,
  {ok, new(Fspath, Welcome)}.

new(Fspath, Welcome) ->
  #hty_static_resource{fspath=Fspath, welcome=Welcome}.

handle(Htx, This) ->
  Fspath = This#hty_static_resource.fspath,
  case Htx:method() of
    'GET' ->
      case Fspath:subpath(Htx:path_below()) of
        ascension_denied ->
          Htx:not_found();
        Fspath1 ->
          case Fspath1:isdir() of
            true ->
              Welcome = This#hty_static_resource.welcome,
              Welcome:list(Htx, Fspath1);
            false ->
              case Fspath1:exists() of
                true ->
                  hty_fileserver:serve(Htx, Fspath1);
                false ->
                  Htx:not_found()
              end
          end
      end;
    _Method -> Htx:method_not_allowed(['GET'])
  end.
