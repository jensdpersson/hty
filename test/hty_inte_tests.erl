-module(hty_inte_tests).

-export([do_test_/0]).

do_test_() ->
  Basedir = "test/testdata/inte/",
  Workdir = Basedir,
  %{ok, _} = file:copy(Basedir, Workdir),
  inets:start(),
  Tests = case file:list_dir(Basedir) of
    {ok, Fixtures} ->
      lists:map(fun(Fixture) ->
        Etcdir = filename:join([Workdir, Fixture, "fixture"]),
        Setup = fun() ->
          {ok, _} = hty_main:start(Etcdir)
        end,
        Teardown = fun(_) ->
          ok = hty_main:stop()
        end,
        {foreach, Setup, Teardown, tests_in_fixture(Workdir, Fixture)}
      end, Fixtures);
    {error, Error} ->
      io:format("Failed running fixtures in ~p, got error ~p~n", [Workdir, Error]),
      {"Create fixtures", fun() -> throw({error, Error}) end}
  end,
  io:format(user, "Reporting tests ~p~n", [Tests]),
  Tests.

tests_in_fixture(Basedir, Fixture) ->
  Fixturedir = filename:join(Basedir, Fixture),
  TestFolder = filename:join(Fixturedir, "tests"),
  case file:list_dir(TestFolder) of
    {ok, Tests} ->
      lists:map(fun(Test) ->
        TestID = Fixture ++ ":" ++ Test,
        {TestID, fun() -> hty_inte_engine:run(filename:join(TestFolder, Test), TestID) end}
      end, Tests);
    {error, enoent} ->
      io:format("Found no tests in ~p~n", [TestFolder]),
      [];
    {error, Error} ->
      Msg = io_lib:format("Fixture ~p got error ~p ~n",
        [Fixture, Error]),
      [fun() -> fail = Msg end]
  end.
