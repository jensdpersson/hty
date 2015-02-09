-module(hty_fs_fs).

-behaviour(hty_fs).

-export([type/1,
         list/1,
         
         send/2,
         recv/3,
         save/2,
         load/1,
         append/2,
         exists/1,
         has_subs/1,
         mkdir/1,
         last_modified/1]).

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

last_modified(Path) ->
  filelib:last_modified(Path).

load(Path) ->
  case file:open(Path, [read, binary]) of
    {ok, Fd} ->
      file:read_file(Fd);
    {error, Error} ->
      {error, Error}
  end.

send(Path, Htx) ->
  Htx:sendfile(Path).

recv(Path, Spafs, Htx) ->
  Htx:recvfile(Path, Spafs).

save(Path, Data) ->
  write(Path, Data, [write, binary]).

append(Path, Data) ->
  write(Path, Data, [append, binary]).

write(Path, Data, Modes) ->
  case file:open(Path, Modes) of
    {ok, Fd} ->
      file:write_file(Fd, Data);
    {error, Error} ->
      {error, Error}
  end.

exists(Path) ->
  filelib:is_file(Path).

has_subs(Path) ->
  filelib:is_dir(Path).

mkdir(Path) ->
  file:make_dir(Path).
