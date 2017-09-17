-module(hty_fspath_tests).

-export([some_params_test/0]).
-export([named_param_est/0]).

some_params_test() ->
  Dut = hty_fspath:new("./parentfolder/1.param1=value1,param2.someext"),
  [{"param1", "value1"}, {"param2", ""}] = Dut:params().

named_param_est() ->
  Dut = hty_fspath:new("./parentfolder/1.param1=value1,param2.someext"),
  "value1" = Dut:param("param1").
