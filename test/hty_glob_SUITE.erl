-module(hty_glob_SUITE).

-export([all/0]).



-export([
    initial_glob_test/1,
    middle_glob_test/1,
    final_glob_test/1,
    final_glob_fail_test/1,
    double_glob_test/1,
    middle_glob_fail_test/1,
    find_explicit_test/1,
    find_glob_test/1,
    mix_folders_and_files_test/1,
    wild_middle_folder_test/1,
    find_double_glob_test/1
]).

all() -> [
  initial_glob_test,
  middle_glob_test,
  final_glob_test,
  final_glob_fail_test,
  double_glob_test,
  middle_glob_fail_test,
  find_explicit_test,
  find_glob_test,
  mix_folders_and_files_test,
  wild_middle_folder_test,
  find_double_glob_test
].

initial_glob_test(_Cfg) -> test_glob("*moore", "roger moore", true).

middle_glob_test(_Cfg) -> test_glob("roger m*re", "roger moore", true).

middle_glob_fail_test(_Cfg) -> test_glob("rog* moore", "roger less", false).

final_glob_test(_Cfg) -> test_glob("blabla*", "blablabla", true).

final_glob_fail_test(_Cfg) -> test_glob("blakbla*", "blablabla", false).

double_glob_test(_Cfg) -> test_glob("*o*", "chimpanzee", false).

test_glob(Glob, Input, Expected) ->
    Expected = hty_glob:glob(Glob, Input),
    ok.

find_explicit_test(Cfg) -> test_find(Cfg, ["apor", "bonobo"], [["apor", "bonobo"]]).
find_glob_test(Cfg) -> test_find(Cfg, ["apor", "*o*"], [
    ["apor", "bonobo"],
    ["apor", "gibbons"],
    ["apor", "gorilla"],
    ["apor", "orangutan"]
]).

find_double_glob_test(Cfg) -> test_find(Cfg, ["**", "h*"],
    [["apor", "gibbons", "hoolock"],
    ["apor", "gibbons", "hylobates"]
]).

mix_folders_and_files_test(Cfg) -> test_find(Cfg, ["apor", "g*"], [
    ["apor", "gibbons"],
    ["apor", "gorilla"]
]).

wild_middle_folder_test(Cfg) -> test_find(Cfg, ["apor", "*", "hylobates"], [
        ["apor", "gibbons", "hylobates"]
]).

test_find(Cfg, Globpath, Expected) ->
    Datadir = proplists:get_value(data_dir, Cfg),
    Fspath = hty_fspath:new(Datadir),
    Actual = hty_glob:find(Globpath, Fspath),
    SortedExpected = lists:sort(Expected),
    SortedExpected = lists:sort(Actual).
