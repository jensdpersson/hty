-module(hty_glob_tests).

-export([
    initial_glob_test/0,
    middle_glob_test/0,
    final_glob_test/0,
    final_glob_fail_test/0,
    double_glob_test/0,
    middle_glob_fail_test/0
]).

-export([
    find_explicit_test/0,
    find_glob_test/0,
    mix_folders_and_files_test/0,
    wild_middle_folder_test/0,
    find_double_glob_test/0
]).

initial_glob_test() -> test_glob("*moore", "roger moore", true).

middle_glob_test() -> test_glob("roger m*re", "roger moore", true).

middle_glob_fail_test() -> test_glob("rog* moore", "roger less", false).

final_glob_test() -> test_glob("blabla*", "blablabla", true).

final_glob_fail_test() -> test_glob("blakbla*", "blablabla", false).

double_glob_test() -> test_glob("*o*", "chimpanzee", false).

test_glob(Glob, Input, Expected) ->
    Expected = hty_glob:glob(Glob, Input).
    
find_explicit_test() -> test_find(["apor", "bonobo"], [["apor", "bonobo"]]).
find_glob_test() -> test_find(["apor", "*o*"], [
    ["apor", "bonobo"],
    ["apor", "gibbons"],
    ["apor", "gorilla"],
    ["apor", "orangutan"]
]).

find_double_glob_test() -> test_find(["**", "h*"], 
    [["apor", "gibbons", "hoolock"],
    ["apor", "gibbons", "hylobates"]
]).

mix_folders_and_files_test() -> test_find(["apor", "g*"], [
    ["apor", "gibbons"],
    ["apor", "gorilla"]
]).

wild_middle_folder_test() -> test_find(["apor", "*", "hylobates"], [
        ["apor", "gibbons", "hylobates"]
]).

test_find(Globpath, Expected) ->
    Fspath = hty_fspath:new("test/testdata/hty_glob"),
    Actual = hty_glob:find(Globpath, Fspath),
    Expected = Actual.
        

