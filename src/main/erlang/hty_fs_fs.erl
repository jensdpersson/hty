-module(hty_fs_fs).

-behaviour(hty_fs).

-export([type/1, list/1, date/1, send/2, recv/3]).

type(Path) ->
  case filelib:is_dir(Path) of
    true -> dir;
    false ->
      case filelib:is_file(Path) of
        true -> file;
        false -> none
      end
  end.

list(Path) ->
  file:list_dir(Path).

date(Path) ->
  filelib:last_modified(Path).

send(Path, Htx) ->
  Htx:sendfile(Path).

recv(Path, Spafs, Htx) ->
  Htx:recvfile(Path, Spafs).
