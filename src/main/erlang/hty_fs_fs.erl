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

load(Path) -> file:read_file(Path).

send(Path, Htx) ->
  Htx:sendfile(Path).

recv(Path, Spafs, Htx) ->
  Htx:recvfile(Spafs, Path).

save(Path, Data) -> file:write_file(Path, Data).

append(Path, Data) ->
  case file:open(Path, [append, binary]) of
    {ok, Fd} ->
      case file:write(Fd, Data) of
        ok ->
          file:close(Fd);
        {error, Error} ->
          {error, {Error, "Failed writing to file"}}
      end;
    {error, Error} ->
      {error, Error}
  end.

exists(Path) ->
  filelib:is_file(Path).

has_subs(Path) ->
  filelib:is_dir(Path).

mkdir(Path) ->
  file:make_dir(Path).
