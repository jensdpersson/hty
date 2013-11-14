-module(hty_xslpi_resource_tests).

-export([simple_test/0,
         choice_test/0
        ]).

simple_test() ->
    Htx = htx([]),
    Dut = dut([{"any", "%2Fxsl%2Ftest.xsl"}]),
    Htx1 = Htx:dispatch([Dut]),
    chk("%2Fxsl%2Ftest.xsl", Htx1).

choice_test() ->
    Htx = htx([{"xslpi_choice", "doc"}]),
    Dut = dut([{"doc", "%2Fxsl%2Ftest.xsl"}, {"error", "%2Fxsl%2Ferror.xsl"}]),
    Htx1 = Htx:dispatch([Dut]),
    chk("%2Fxsl%2Ftest.xsl", Htx1).

dut(Paths) ->
    hty_xslpi_resource:new(Paths, []).

htx(BindMe) ->
    Htx = hty_tx_factory:new(),
    lists:foldl(fun({K,V}, A) -> A:bind(K,V) end, Htx, BindMe).

chk(Xslpi, Htx) -> 
    [<<"<?xml-stylesheet type=\"text/xsl\" href=\"">>,
           Xslpi,<<".xsl\"?>">>|_] = Htx:outs().