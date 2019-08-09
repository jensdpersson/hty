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
  Welcome = case hty_fspath:param("welcome", Fspath) of
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
  case hty_tx:method(Htx) of
    'GET' ->
      io:format("static path below [~p,~p,~p]~n",
        [hty_tx:path_below(Htx),
         hty_percentencoding:decode(hty_tx:path_below(Htx)),
         hty_percentencoding:decode_each(hty_tx:path_below(Htx))
        ]),
      Filepath = hty_percentencoding:decode_each(hty_tx:path_below(Htx)),
      case hty_fspath:subpath(Filepath, Fspath) of
        ascension_denied ->
          hty_tx:not_found(Htx);
        Fspath1 ->
          io:format("static subpath [~p]~n", [Fspath1]),
          case hty_fspath:isdir(Fspath1) of
            true ->
              Welcome = This#hty_static_resource.welcome,
              hty_welcome:invoke_list(Htx, Fspath1, Welcome);
            false ->
              case hty_fspath:exists(Fspath1) of
                true ->
                  hty_fileserver:serve(Htx, Fspath1);
                false ->
                  hty_tx:not_found(Htx)
              end
          end
      end;
    _Method -> hty_tx:method_not_allowed(['GET'], Htx)
  end.
