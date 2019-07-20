-module(hty_override_resource_tests).

-export([get_root_test/0,
         override_test/0,
         fail_on_other_filename_test/0,
         fail_on_other_filename_deep_test/0]).

htx(Path) ->
    H = hty_tx_factory:get(Path).
%  H1 = H:method('GET'),
%  H2 = H1:path({[], Path}),
%  H2.

dut() ->
    Folder = hty_fspath:new("test/testdata/hty_override_resource"),
    hty_override_resource:new("data.txt", Folder).

get_root_test() ->
    Htx = htx(<<"data.txt">>),
    Dut = dut(),
    Htx1 = hty_tx:dispatch([Dut], Htx),
    {200, _} = hty_tx:status(Htx1),
    [{file, "test/testdata/hty_override_resource/data.txt"}|_] = hty_tx:outs(Htx1).

override_test() ->
    Htx = htx(<<"sub1/data.txt">>),
    Dut = dut(),
    Htx1 = hty_tx:dispatch([Dut], Htx),
    {200, _} = hty_tx:status(Htx1),
    [{file, "test/testdata/hty_override_resource/sub1/data.txt"}|_] = hty_tx:outs(Htx1).

fail_on_other_filename_test() ->
    Htx = htx(<<"otherfile.txt">>),
    Dut = dut(),
    Htx1 = hty_tx:dispatch([Dut], Htx),
    {404, _} = hty_tx:status(Htx1).

fail_on_other_filename_deep_test() ->
    Htx = htx(<<"sub1/otherfile.txt">>),
    Dut = dut(),
    Htx1 = hty_tx:dispatch([Dut], Htx),
    {404, _} = hty_tx:status(Htx1).
