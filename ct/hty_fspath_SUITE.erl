-module(hty_fspath_SUITE).
-compile(export_all).

all() -> [
    {group, move},
    {group, copy}
].

groups() -> [
  {move, [
    move_direct_child_file,
    move_direct_child_folder,
    move_nested_source
  ]},
  {copy, [
    copy_direct_child_file,
    copy_direct_child_folder,
    copy_nested_source,
    copy_missing_dst_folder,
    copy_preexisting_dst_file
  ]}
].


-record(test_move_or_copy, {mode, xpct, from, dest}).

move_direct_child_file(Cfg) ->
  test_move_or_copy(Cfg, #test_move_or_copy{
      mode = move,
      xpct = ok,
      from = "move_direct_child_file_src",
      dest = "move_direct_child_file_dst"
  }).

move_direct_child_folder(Cfg) -> 
    test_move_or_copy(Cfg, #test_move_or_copy{
      mode = move,
      xpct = ok,
      from = "move_direct_child_folder_src",
      dest = "move_direct_child_folder_dst"
  }).
    
move_nested_source(Cfg) -> 
    test_move_or_copy(Cfg, #test_move_or_copy{
      mode = move,
      xpct = ok,
      from = "move_nested_source_src/file2move",
      dest = "move_nested_source_dst/file2move"
  }).

copy_direct_child_file(Cfg) ->
  test_move_or_copy(Cfg, #test_move_or_copy{
      mode = copy,
      xpct = {ok, 3},
      from = "copy_direct_child_file_src",
      dest = "copy_direct_child_file_dst"
  }).
  
copy_direct_child_folder(Cfg) -> 
    test_move_or_copy(Cfg, #test_move_or_copy{
      mode = copy,
      xpct = ok,
      from = "copy_direct_child_folder_src",
      dest = "copy_direct_child_folder_dst"
  }).
    
copy_nested_source(Cfg) -> 
    test_move_or_copy(Cfg, #test_move_or_copy{
      mode = copy,
      xpct = {ok, 6},
      from = "copy_nested_source_src/file2copy",
      dest = "copy_nested_source_dst/file2copy"
  }).
    
copy_missing_dst_folder(Cfg) ->
    test_move_or_copy(Cfg, #test_move_or_copy{
      mode = copy,
      xpct = ok,
      from = "copy_direct_child_folder_src",
      dest = "copy_missing_dst_folder/newfile"
  }).
  
copy_preexisting_dst_file(Cfg) ->
    test_move_or_copy(Cfg, #test_move_or_copy{
      mode = copy,
      xpct = {error, eexist},
      from = "copy_existing_file",
      dest = "copy_existing_file_dst"
  }).

test_move_or_copy(Cfg, #test_move_or_copy{from=From, dest=To, mode=MoveOrCopy, xpct=Expect}) ->
  Filesdir = proplists:get_value(files_dir, Cfg),
  From1 = Filesdir ++ From,
  To1 = Filesdir ++ To,
  io:format("Fromfile is [~p] and Tofile is [~p]~n", [From1, To1]),
  %true = filelib:is_file(From1),
  %false = filelib:is_file(To1),
  Frompath = hty_fspath:new(From1),
  Topath = hty_fspath:new(To1),
  case MoveOrCopy of
  % TODO recursive compare
    move ->
        Expect = hty_fspath:move(Frompath, Topath),
        true = filelib:is_file(To1),
        false = filelib:is_file(From1);
    copy ->
        Expect = hty_fspath:copy(Frompath, Topath),
        true = filelib:is_file(To1),
        true = filelib:is_file(From1)
  end.

init_per_group(Grp, Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    Fromdir = Datadir ++ atom_to_list(Grp) ++ "/files/",
    Todir = proplists:get_value(priv_dir, Config) ++ "/hty_fspath/" ++ atom_to_list(Grp) ++ "/files/",
    copy_fixture(Fromdir, Todir),
    [{files_dir, Todir}|Config].

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
