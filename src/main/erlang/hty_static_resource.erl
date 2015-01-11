%% Author: jens
%% Created: 18 dec 2012
%% Description: TODO: Add description to hty_public_resource
-module(hty_static_resource).

-record(hty_static_resource, {fspath}).


%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/1]).


%%
%% API Functions
%%
new(Fspath) ->
  #hty_static_resource{fspath=Fspath}.

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
                  F=fun(W) ->
                    Fspath2 = Fspath1:subpath([W]),
                    io:format("Does ~p exist?~n", [Fspath2:filepath()]),
                    case Fspath2:exists() of
                      true -> [Fspath2];
                      false -> []
                    end
                  end,
                  L = ["index.html", "index.xml"],
                  case lists:flatmap(F, L) of
                    [Welcome|_] ->
                      serve(Htx, Welcome);
                      [] ->
                        Htx:not_found()
                      end;
                      false ->
                        case Fspath1:exists() of
                          true ->
                            serve(Htx, Fspath1);
                            false ->
                              Htx:not_found()
                            end
                          end
                        end;
                        _Method -> Htx:method_not_allowed(['GET'])
                      end.


%%
%% Local Functions
%%
make_etag(Fs) -> [$"] ++ Fs:last_modified() ++ [$"].

serve(Htx0, Fs) ->
  ETag = make_etag(Fs),
  BTag = list_to_binary(ETag),
  case Htx0:req_header('If-None-Match') of
    [BTag] ->
      io:format("ETag Match: ~p~n", [ETag]),
      Htx01 = Htx0:rsp_header("ETag", ETag),
      Htx01:not_modified();
      SomethingElse ->
        io:format("If-None-Match: ~p!=~p~n", [SomethingElse,ETag]),
        Htx01 = Htx0:rsp_header("ETag", ETag),
        Htx = Htx01:rsp_header("Cache-Control", "public"),
        Mime = Htx:mimemap(Fs:ext()),
        Htx2 = Htx:rsp_header('Content-Type', Mime),
        Htx3 = Htx2:sendfile(Fs:filepath()),
        Htx3:ok()
      end.
