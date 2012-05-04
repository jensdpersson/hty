-module(hty_walker_test).

single_matching_rule_test() ->
    Rules = [basic_rule:new()],
    hty_walker:new().


run(Rules, Facit) ->
    Walker = hty_walker:new(Rules).
    


%hmm. Kaputt stdin/stdout protocol? Eller verdict.xml?
%Porjektet apport bygger väl verdict-xml?
%Kör på apport. 
