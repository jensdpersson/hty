%% @author jens
%% @doc @todo Add description to hty_xslpi_rule_tests.


-module(hty_xslpi_rule_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([complex_test/0]).

complex_test() ->
	Filename = "content.doc=%2Fentity,error=%2F500.xslpi",
	no = Filename.
	
%% ====================================================================
%% Internal functions
%% ====================================================================


