-module(hty_json_spaf_SUITE).
-compile(export_all).

all() -> [
  {group, format}
].

groups() -> [
  {format, [
    empty_element,
    empty_attribute,
    element_with_text,
    empty_element_with_attributes,
    element_with_text_and_attributes,
    element_with_elements_with_different_names,
    element_with_elements_with_identical_names,
    element_with_elements_and_text%,
    %element_with_attributes_subelements_and_text
  ]}
].


% <e/>
empty_element(_Cfg) ->
    do_test([{push,<<"e">>},{pop,<<"e">>}], <<"{\"e\":[]}">>).

empty_attribute(_Cfg) ->
    do_test([{push,<<"@e">>},{pop,<<"@e">>}], <<"{\"@e\":null}">>).

% <e>t</e>
element_with_text(_Cfg) ->
    do_test(
        [{push,<<"e">>},{text,<<"t">>},{pop,<<"e">>}],
        <<"{\"e\":[\"t\"]}">>
    ).

% <e name="value" />
empty_element_with_attributes(_Cfg) ->
    do_test(
        [{push,<<"e">>},{push,<<"@name">>},{text,<<"value">>},{pop,<<"@name">>},{pop,<<"e">>}],
        <<"{\"e\":[{\"@name\":\"value\"}]}">>
    ).

%  <e name="value">text</e>
element_with_text_and_attributes(_Cfg) -> do_test(
        [{push,<<"e">>},
         {push,<<"@name">>},
         {text,<<"value">>},
         {pop,<<"@name">>},
         {text,<<"content">>},
         {pop,<<"e">>}],
        <<"{\"e\":[{\"@name\":\"value\"},\"content\"]}">>
    ).
% 	push-e,push-a,text-text,pop-a,push-b,text-text,pop-b,pop-b <e> <a>text</a> <b>text</b> </e> 	"e": { "a": "text", "b": "text" }
element_with_elements_with_different_names(_Cfg) -> do_test(
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
element_with_elements_with_identical_names(_Cfg) -> do_test(
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
element_with_elements_and_text(_Cfg) -> do_test(
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
element_with_attributes_subelements_and_text(_Cfg) -> y=n.

do_test(Evtlist, Expected) ->
    {_, Actual} = lists:foldl(fun(Evt, {State, Outs}) ->
        {ok, State1, Out} = hty_json_spaf:format(Evt, State),
        {State1, [Out|Outs]}
    end, {q0, []}, Evtlist),
    Expected = list_to_binary(lists:reverse(Actual)).
