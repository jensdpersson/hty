%% Author: jens
%% Created: 18 dec 2012
%% Description:
-module(hty_sendfile_resource).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, mount/2]).
-record(hty_sendfile_resource, {mime, fspath}).
%%
%% API Functions
%%
mount(Fspath, _Mc) ->

  case hty_fspath:type(Fspath) of
    file ->
      case lists:reverse(hty_fspath:parts(Fspath)) of
        ["sendfile", Param|_] ->
          Mime = binary_to_list(hty_percentencoding:decode(Param)),
          {ok, #hty_sendfile_resource{mime=Mime, fspath=Fspath}};
        _ ->
          {error, "sendfile resource requires a mime type param"}
      end;
    none ->
      {error, {"path given for sendfile resource does not exist", hty_fspath:path(Fspath)}};
    dir ->
      {error, {"path given for sendfile resource is a folder, not a file", hty_fspath:path(Fspath)}}
  end.

handle(Htx, This) ->
    case hty_tx:method(Htx) of
	'GET' ->
	    Htx1 = hty_tx:rsp_header('Content-Type', This#hty_sendfile_resource.mime, Htx),
	    %Basename = Fspath:basename(),
	    %io:format("Basename(~p) =? PathBelow(~p)~n",[Basename, Htx:path_below()]),
	    %case Htx:path_below() of
		%[Basename] ->
	    Fspath = This#hty_sendfile_resource.fspath,
      hty_tx:with([
        ok,
        commit,
	      {sendfile, hty_fspath:path(Fspath)}
      ], Htx1);
		%_ ->
		%    Htx:not_found()
	    %end;
	_Method ->
	    hty_tx:method_not_allowed(['GET'], Htx)
    end.

%%
%% Local Functions
%%
