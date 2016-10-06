%% Author: jens
-module(hty_history_resource).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([mount/1, handle/2, new/2]).
-record(hty_history_resource, {fspath, diskformat}).

mount(Fspath) ->
  case lists:reverse(Fspath:parts()) of
    ["history", Diskformat | _] ->
      {ok, new(Fspath, Diskformat)};
    _ ->
      {ok, new(Fspath, "xml")}
  end.

new(Fspath, DiskFormat) ->
  #hty_history_resource{fspath=Fspath, diskformat=DiskFormat}.

handle(Htx0, This) ->
  Method = Htx0:method(),
  Fspath = This#hty_history_resource.fspath,
  DiskFormat = This#hty_history_resource.diskformat,
  case Htx0:path_below() of
    [] ->
      case Method of
        'GET' ->
          Htx1 = Htx0:bind("xslpi_choice", "list"),
          Htx2 = hty_listing:list(Htx1, Fspath, [last_modified]),
          Htx2:commit();
        'POST' ->
          Htx0:method_not_allowed(['GET'])
      end;
    [Segment] ->
      Htx = Htx0:bind("xslpi_choice", "doc"),
      Fspath1 = Fspath:subpath([Segment]),
      Rev = Htx:matrix(<<"rev">>),
      io:format("Processing ~p", [Segment]),
      case Rev of
        no ->
          io:format("Rev is [~p]~n", [Rev]),
          case tip(Fspath1) of
            nofile ->
              case Method of
                'GET' ->
                  Htx:not_found();
                'POST' ->
                  Fspath1:mkdir(),
                  save(Htx, "0", Fspath1, DiskFormat)
              end;
            notip ->
              redirect(Htx, "0");
            noread ->
              Htx:server_error("FailedReadingTip");
            Tip ->
              redirect(Htx, Tip)
          end;
      _ ->
        io:format(";rev=~p~n", [Rev]),
        Fspath2 = Fspath1:subpath([binary_to_list(Rev) ++ ".data"]),
        case Method of
          'GET' ->
            case Fspath2:exists() of
              true ->
                Htx1 = (Htx:ok()):commit(),
                Htx2 = Htx1:rsp_header("Content-Type", "application/xml"),
                Fspath2:send(Htx2);
              false ->
                case tip(Fspath1) of
                  nofile ->
                    Htx:not_found();
                  notip ->
                    Htx:not_found();
                  noread ->
                    Htx:server_error("FailedReadingTip");
                  Tip ->
                    redirect(Htx, Tip)
                end
            end;
          'POST' ->
            case tip(Fspath1) of
              nofile ->
                Htx:not_found();
              notip ->
                Htx:not_found();
              noread ->
                Htx:server_error("FailedReadingTip");
              Rev ->
                Int = list_to_integer(binary_to_list(Rev)),
                Int1 = Int + 1,
                Rev1 = integer_to_list(Int1),
                save(Htx, Rev1, Fspath1, DiskFormat);
              Num ->
                io:format("Rev does not match tip: [~p != ~p]", [Rev, Num]),
                conflict(Htx, Num, Rev)
            end;
          _ ->
            Htx:method_not_allowed(['GET', 'POST'])
        end
      end
  end.

conflict(Htx, _Tip, _Rev) ->
  Htx1 = Htx:conflict(),
  Htx1.

redirect(Htx, Tip) ->
  Location = hty_uri:matrix(Htx:path(), <<"rev">>, Tip),
  Htx1 = Htx:temporary_redirect(hty_uri:pack(Location)),
  Htx1:commit().

save(Htx, Tip, Fs, DiskFormat) ->
  Spaf = spaf(Htx:req_header('Content-Type'), DiskFormat),
  Datafile = Fs:subpath([Tip ++ ".data"]),
  Htx1 = Datafile:recv(Spaf, Htx),
  Tipfile = Fs:subpath(["tip"]),
  case Tipfile:save(Tip) of
    {error, Error} ->
      Htx1:server_error(Error);
    ok ->
      Location = hty_uri:matrix(Htx1:path(), <<"rev">>, Tip),
      Htx2 = Htx1:see_other(hty_uri:pack(Location)),
      Htx2:commit()
  end.

tip(Fs) ->
  case Fs:exists() of
    false ->
      nofile;
    true ->
      Fs1 = Fs:subpath(["tip"]),
      case Fs1:exists() of
        false ->
          notip;
        true ->
          case Fs1:load() of
            {ok, Content} ->
              Content;
            {error, Error} ->
              io:format("FailedReadingTip ~p ~p", [Fs1, Error]),
              noread
          end
      end
  end.

spaf([<<"application/x-www-form-urlencoded">>], "xml") ->
    [fun hty_formtree_spaf:parse/2, fun hty_xml_spaf:format/2];
spaf(Mime, Diskf) ->
    io:format("Unregged mime/diskformat combo [~p/~p]~n", [Mime, Diskf]).
