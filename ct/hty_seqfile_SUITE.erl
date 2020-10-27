-module(hty_seqfile_SUITE).

-export([
  all/0,
  init_per_suite/1,
  init_test/1,
  incr_test/1,
  badfile_test/1
]).

all() -> [
  init_test,
  incr_test,
  badfile_test
].

init_per_suite(Cfg) -> filelib:ensure_dir(priv_dir(Cfg)), Cfg.

priv_dir(Cfg) -> proplists:get_value(priv_dir, Cfg) ++ "hty_seqfile/".

init_test(Cfg) ->
  {ok, "1"} = hty_seqfile:incr(priv_dir(Cfg) ++ "newfile.txt").

incr_test(Cfg) ->
  {ok, N} = hty_seqfile:incr(priv_dir(Cfg) ++ "newerfile.txt"),
  {N1, _} = string:to_integer(N),
  N2 = integer_to_list(N1 + 1),
  {ok, N2} = hty_seqfile:incr(priv_dir(Cfg) ++ "newerfile.txt").

badfile_test(Cfg) ->
    File = priv_dir(Cfg) ++ "badfolder/badfile.txt",
    {err, File, enoent} = hty_seqfile:incr(File).
