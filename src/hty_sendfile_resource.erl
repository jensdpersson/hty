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
  case Fspath:type() of
    file ->
      case lists:reverse(Fspath:parts()) of
        ["sendfile", Param|_] ->
          Mime = binary_to_list(hty_percentencoding:decode(Param)),
          {ok, #hty_sendfile_resource{mime=Mime, fspath=Fspath}};
        _ ->
          {error, "sendfile resource requires a mime type param"}
      end;
    none ->
      {error, {"path given for sendfile resource does not exist", Fspath:path()}};
    dir ->
      {error, {"path given for sendfile resource is a folder, not a file", Fspath:path()}}
  end.

handle(Htx, This) ->
    case Htx:method() of
	'GET' ->
	    Htx1 = Htx:rsp_header('Content-Type', This#hty_sendfile_resource.mime),
	    %Basename = Fspath:basename(),
	    %io:format("Basename(~p) =? PathBelow(~p)~n",[Basename, Htx:path_below()]),
	    %case Htx:path_below() of
		%[Basename] ->
	    Fspath = This#hty_sendfile_resource.fspath,
      Htx2 = Htx1:ok(),
      Htx3 = Htx2:commit(),
	    Htx3:sendfile(Fspath:path());
		%_ ->
		%    Htx:not_found()
	    %end;
	_Method ->
	    Htx:method_not_allowed(['GET'])
    end.

%%
%% Local Functions
%%
