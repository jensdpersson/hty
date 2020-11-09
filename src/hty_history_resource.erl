%% Author: jens
-module(hty_history_resource).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([mount/2, handle/2, new/2]).
-record(hty_history_resource, {fspath, diskformat}).

mount(Fspath, _Mc) ->
  case lists:reverse(hty_fspath:parts(Fspath)) of
    ["history", Diskformat | _] ->
      {ok, new(Fspath, Diskformat)};
    _ ->
      {ok, new(Fspath, "xml")}
  end.

new(Fspath, DiskFormat) ->
  #hty_history_resource{fspath=Fspath, diskformat=DiskFormat}.

handle(Htx0, This) ->
  io:format("history:handle()", []),
  Method = hty_tx:method(Htx0),
  Fspath = This#hty_history_resource.fspath,
  DiskFormat = This#hty_history_resource.diskformat,
  case hty_tx:path_below(Htx0) of
    [] ->
      case Method of
        'GET' ->
          io:format("history:leaf.get()", []),
          Htx1 = hty_tx:bind("xslpi_choice", "list", Htx0),
          Htx2 = hty_listing_welcome:list(Htx1, Fspath, [last_modified]),
          hty_tx:commit(Htx2);
        'POST' ->
          hty_tx:method_not_allowed(['GET'], Htx0)
      end;
    [Segment] ->
      io:format("history:twig()", []),
      Htx = hty_tx:bind("xslpi_choice", "doc", Htx0),
      Fspath1 = hty_fspath:subpath([Segment], Fspath),
      Rev = hty_tx:matrix(<<"rev">>, Htx),
      io:format("Processing ~p", [Segment]),
      case Rev of
        no ->
          io:format("Rev is [~p]~n", [Rev]),
          case tip(Fspath1) of
            nofile ->
              case Method of
                'GET' ->
                  hty_tx:not_found(Htx);
                'POST' ->
                  hty_fspath:mkdir(Fspath1),
                  save(Htx, "0", Fspath1, DiskFormat)
              end;
            notip ->
              redirect(Htx, "0");
            noread ->
              hty_tx:server_error("FailedReadingTip", Htx);
            Tip ->
              redirect(Htx, Tip)
          end;
      _ ->
        io:format(";rev=~p~n", [Rev]),
        Fspath2 = hty_fspath:subpath([binary_to_list(Rev) ++ ".data"], Fspath1),
        case Method of
          'GET' ->
            case hty_fspath:exists(Fspath2) of
              true ->
                Htx1 = hty_tx:commit(hty_tx:ok(Htx)),
                Htx2 = hty_tx:rsp_header("Content-Type", "application/xml", Htx1),
                hty_fspath:send(Htx2, Fspath2);
              false ->
                case tip(Fspath1) of
                  nofile ->
                    hty_tx:not_found(Htx);
                  notip ->
                    hty_tx:not_found(Htx);
                  noread ->
                    hty_tx:server_error("FailedReadingTip", Htx);
                  Tip ->
                    redirect(Htx, Tip)
                end
            end;
          'POST' ->
            case tip(Fspath1) of
              nofile ->
                hty_tx:not_found(Htx);
              notip ->
                hty_tx:not_found(Htx);
              noread ->
                hty_tx:server_error("FailedReadingTip", Htx);
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
            hty_tx:method_not_allowed(['GET', 'POST'], Htx)
        end
      end
  end.

conflict(Htx, _Tip, _Rev) ->
  Htx1 = hty_tx:conflict(Htx),
  Htx1.

redirect(Htx, Tip) ->
  Location = hty_uri:matrix(hty_tx:path(Htx), <<"rev">>, Tip),
  Htx1 = hty_tx:temporary_redirect(hty_uri:pack(Location), Htx),
  hty_tx:commit(Htx1).

save(Htx, Tip, Fs, DiskFormat) ->
  case spaf(hty_tx:req_header('Content-Type', Htx), DiskFormat) of
    error ->
      io:format("Headers ~p ~n", [hty_tx:req_headers(Htx)]),
      hty_tx:commit(hty_tx:bad_request(Htx));
    Spaf ->
      Datafile = hty_fspath:subpath([Tip ++ ".data"], Fs),
      Htx1 = hty_fspath:recv(Spaf, Htx, Datafile),
      io:format("Path1 = ~p, Path2 = ~p~n", [hty_tx:path(Htx), hty_tx:path(Htx1)]),
      io:format("Tx1 = ~p, Tx2 = ~p~n", [Htx, Htx1]),
      case hty_tx:status(Htx1) of
        {400, _} -> Htx1;
        _ ->
          Tipfile = hty_fspath:subpath(["tip"], Fs),
          case hty_fspath:save(Tip, Tipfile) of
            {error, Error} ->
              io:format("Failed saving tip~n"),
              hty_tx:server_error(Error, Htx1);
            ok ->
              Location = hty_uri:matrix(hty_tx:path(Htx1), <<"rev">>, Tip),
              Htx2 = hty_tx:see_other(hty_uri:pack(Location), Htx1),
              hty_tx:commit(Htx2)
          end
      end
  end.

tip(Fs) ->
  case hty_fspath:exists(Fs) of
    false ->
      nofile;
    true ->
      Fs1 = hty_fspath:subpath(["tip"], Fs),
      case hty_fspath:exists(Fs1) of
        false ->
          notip;
        true ->
          case hty_fspath:load(Fs1) of
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
    io:format("Unregged mime/diskformat combo [~p/~p]~n", [Mime, Diskf]),
    error.
