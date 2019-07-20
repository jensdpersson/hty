-module(hty_fspath_tests).

-export([some_params_test/0]).
-export([named_param_est/0]).

some_params_test() ->
  Cfg = hty_fspath:new("./parentfolder/1.param1=value1,param2.someext"),
  [{"param1", "value1"}, {"param2", ""}] = hty_fspath:params(Cfg).

named_param_est() ->
  Cfg = hty_fspath:new("./parentfolder/1.param1=value1,param2.someext"),
  "value1" = hty_fspath:param("param1", Cfg).
