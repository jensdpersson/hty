-module(hty_json_spaf_tests).
-export([
    empty_element_test/0,
    empty_attribute_test/0,
    element_with_text_test/0,
    empty_element_with_attributes_test/0,
    element_with_text_and_attributes_test/0,
    element_with_elements_with_different_names_test/0,
    element_with_elements_with_identical_names_test/0,
    element_with_elements_and_text_test/0,
    element_with_attributes_subelements_and_text/0
]).

% <e/>
empty_element_test() -> 
    do_test([{push,<<"e">>},{pop,<<"e">>}], <<"{\"e\":[]}">>).
    
empty_attribute_test() -> 
    do_test([{push,<<"@e">>},{pop,<<"@e">>}], <<"{\"@e\":null}">>).

% <e>t</e>
element_with_text_test() -> 
    do_test(
        [{push,<<"e">>},{text,<<"t">>},{pop,<<"e">>}], 
        <<"{\"e\":[\"t\"]}">>
    ).

% <e name="value" />
empty_element_with_attributes_test() -> 
    do_test(
        [{push,<<"e">>},{push,<<"@name">>},{text,<<"value">>},{pop,<<"@name">>},{pop,<<"e">>}],
        <<"{\"e\":[{\"@name\":\"value\"}]}">>
    ).

%  <e name="value">text</e>
element_with_text_and_attributes_test() -> do_test(
        [{push,<<"e">>},
         {push,<<"@name">>},
         {text,<<"value">>},
         {pop,<<"@name">>},
         {text,<<"content">>},
         {pop,<<"e">>}],
        <<"{\"e\":[{\"@name\":\"value\"},\"content\"]}">>
    ).
% 	push-e,push-a,text-text,pop-a,push-b,text-text,pop-b,pop-b <e> <a>text</a> <b>text</b> </e> 	"e": { "a": "text", "b": "text" }
element_with_elements_with_different_names_test() -> do_test(
        [{push,<<"e">>},
         {push,<<"a">>},
         {text,<<"t">>},
         {pop,<<"a">>},
         {push,<<"b">>},
         {text,<<"t2">>},
         {pop,<<"b">>},
         {pop,<<"e">>}],
        <<"{\"e\":[{\"a\":[\"t\"]},{\"b\":[\"t2\"]}]}">>
    ).
% 	<e> <a>text</a> <a>text</a> </e>
element_with_elements_with_identical_names_test() -> do_test(
        [{push,<<"e">>},
         {push,<<"a">>},
         {text,<<"t">>},
         {pop,<<"a">>},
         {push,<<"a">>},
         {text,<<"t2">>},
         {pop,<<"a">>},
         {pop,<<"e">>}],
        <<"{\"e\":[{\"a\":[\"t\"]},{\"a\":[\"t2\"]}]}">>
    ).
    
% 	<e> text <a>text</a> </e>
element_with_elements_and_text_test() -> do_test(
        [{push,<<"e">>},
         {text,<<"t">>},    
         {push,<<"a">>},
         {text,<<"t2">>},
         {pop,<<"a">>},
         {pop,<<"e">>}],
        <<"{\"e\":[\"t\",{\"a\":[\"t2\"]}]}">>
    ).

% <project name="apa">
%   <!-- needed? --> 
%   <dep name="annan_apa"/>    
%   % why? 
%   <dep name="tredje_apa"/>    
% </project>
element_with_attributes_subelements_and_text() -> n=y.    

do_test(Evtlist, Expected) ->
    {_, Actual} = lists:foldl(fun(Evt, {State, Outs}) ->
        {ok, State1, Out} = hty_json_spaf:format(Evt, State),
        {State1, [Out|Outs]}
    end, {q0, []}, Evtlist),
    Expected = list_to_binary(lists:reverse(Actual)).






