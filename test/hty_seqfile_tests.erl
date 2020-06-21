-module(hty_seqfile_tests).
-export([init_test/0, incr_test/0, badfile_test/0]).

init_test() ->
    file:delete("newfile.txt"),
    {ok, "1"} = hty_seqfile:incr("newfile.txt").

incr_test() ->
    file:delete("newerfile.txt"),
    {ok, N} = hty_seqfile:incr("newerfile.txt"),
    {N1, _} = string:to_integer(N),
    N2 = integer_to_list(N1 + 1),
    {ok, N2} = hty_seqfile:incr("newerfile.txt").
    
badfile_test() ->
    File = "badfolder/badfile.txt",
    {err, File, enoent} = hty_seqfile:incr(File).
    