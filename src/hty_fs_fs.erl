-module(hty_fs_fs).

-behaviour(hty_fs).

-export([type/1,
         list/1,
         collect/2,
         send/2,
         recv/3,
         save/2,
         load/1,
         append/2,
         exists/1,
         has_subs/1,
         mkdir/1,
         delete/1,
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
    Ret = filelib:last_modified(Path),
    io:format("last_modified [~p] is [~p]~n", [Path, Ret]),
    Ret.


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

delete(Path) ->
  case filelib:is_dir(Path) of
    true ->
      io:format("~p is a folder~n", [Path]),
        case file:list_dir(Path) of
          {ok, Files} ->
            lists:foreach(fun(Sub) -> delete(Path ++ [$/|Sub]) end, Files),
          case file:del_dir(Path) of
                {error, Error2} ->
                  io:format("Failed removing ~p ~p~n", [Path, Error2]),
                  {no, Error2};
                ok -> ok
              end;
            {error, Error} ->
              io:format("Failed listing ~p ~p~n", [Path, Error]),
              {no, Error}
          end;
        false ->
      case file:delete(Path) of
        {error, Error} ->
          io:format("Failed deleting ~p ~p~n", [Path, Error]),
          {no, Error};
        ok ->
          ok
      end
  end.

collect(Path, Pred) ->
  case file:open(Path, [read, binary]) of
    {ok, Fd} ->
      case collect_line(Fd, Pred, []) of
        {error, Error} ->
          {error, Error};
        Lines ->
          file:close(Fd),
          Lines
      end;
    {error, Error} ->
      {error, Error}
  end.

collect_line(Fd, Pred, Sofar) ->
  case io:get_line(Fd, "") of
    eof ->
      Sofar;
    {error, Error} ->
      {error, Error};
    Data ->
      collect_line(Fd, Pred, [Pred(Data)|Sofar])
  end.
