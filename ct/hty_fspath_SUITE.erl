-module(hty_fspath_SUITE).
-compile(export_all).

all() -> [
    {group, move}
].

groups() -> [
  {move, [
    move_direct_child_file,
    move_direct_child_folder,
    move_nested_source
  ]}
].

move_direct_child_file(Cfg) ->
  test_move(Cfg, "move_direct_child_file_src", "move_direct_child_file_dst").

move_direct_child_folder(Cfg) -> 
    test_move(Cfg, "move_direct_child_folder_src",  "move_direct_child_folder_dst").
    
move_nested_source(Cfg) -> 
    test_move(Cfg, "move_nested_source_src/file2move", "move_nested_source_dst/file2move").

test_move(Cfg, From, To) ->
  Filesdir = proplists:get_value(files_dir, Cfg),
  From1 = Filesdir ++ From,
  To1 = Filesdir ++ To,
  io:format("Fromfile is [~p] and Tofile is [~p]~n", [From1, To1]),
  true = filelib:is_file(From1),
  false = filelib:is_file(To1),
  Frompath = hty_fspath:new(From1),
  Topath = hty_fspath:new(To1),
  ok = hty_fspath:move(Frompath, Topath),
  true = filelib:is_file(To1),
  false = filelib:is_file(From1).

init_per_group(move, Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    Fromdir = Datadir ++ "move/files/",
    Todir = proplists:get_value(priv_dir, Config) ++ "move/files/",
    copy_fixture(Fromdir, Todir),
    [{files_dir, Todir}|Config];

init_per_group(_, Cfg) -> Cfg.

end_per_group(_Group, Config) ->
    Config.

%%% Support
copy_fixture(From, To) ->
    case filelib:is_dir(From) of
        true ->
            filelib:ensure_dir(To),
            file:make_dir(To),
            {ok, Filenames} = file:list_dir(From),
            lists:foreach(fun(Filename) ->
                copy_fixture(From ++ "/" ++ Filename, To ++ "/" ++ Filename)
            end, Filenames);
        false ->
            ok = filelib:ensure_dir(To),
            io:format("Copying from ~s to ~s", [From, To]),
            {ok, _} = file:copy(From, To)
    end.
