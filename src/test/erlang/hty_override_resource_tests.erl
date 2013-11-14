-module(hty_override_resource_tests).

-export([get_root_test/0,
         override_test/0,
         fail_on_other_filename_test/0,
         fail_on_other_filename_deep_test/0]).

htx(Path) ->
    H = hty_tx_factory:new(),
    H1 = H:method('GET'),
    H2 = H1:path({[], Path}),
    H2.

dut() ->
    Folder = hty_fs_cursor:new("src/test/testdir/hty_override_resource"),
    hty_override_resource:new("data.txt", Folder).
    
get_root_test() ->
    Htx = htx(["data.txt"]),
    Dut = dut(),
    Htx1 = Htx:dispatch([Dut]),
    {200, _} = Htx1:status(),
    [{file, "src/test/testdir/hty_override_resource/data.txt"}|_] = Htx1:outs().

override_test() ->
    Htx = htx(["sub1", "data.txt"]),
    Dut = dut(),
    Htx1 = Htx:dispatch([Dut]),
    {200, _} = Htx1:status(),
    [{file, "src/test/testdir/hty_override_resource/sub1/data.txt"}|_] = Htx1:outs().

fail_on_other_filename_test() ->
    Htx = htx(["otherfile.txt"]),
    Dut = dut(),
    Htx1 = Htx:dispatch([Dut]),
    {404, _} = Htx1:status().
    
fail_on_other_filename_deep_test() ->
    Htx = htx(["sub1", "otherfile.txt"]),
    Dut = dut(),
    Htx1 = Htx:dispatch([Dut]),
    {404, _} = Htx1:status().