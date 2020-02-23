-module(hty_json_spaf).

-export([format/2]).

% Json spaf

% 	push-e,pop-e  <e/> 	"e": null 	
% 	push-e,text-t,pop-e <e>t</e> 	"e": "t"
% 	push-e,push-@name,text-value,pop-e   <e name="value" /> 	"e":{"@name": "value"}
% 	push-e,push-@name,text-value,text-t,pop-e   <e name="value">text</e> 	"e": { "@name": "value", "#text": "text" }
% 	push-e,push-a,text-text,pop-a,push-b,text-text,pop-b,pop-b <e> <a>text</a> <b>text</b> </e> 	"e": { "a": "text", "b": "text" }
% 	push-e,push-a,text-text,pop-a,push-a,text-text,pop-a,pop-e  <e> <a>text</a> <a>text</a> </e> 	"e": { "a": ["text", "text"] }
% 	push-e,text-text,push-a,text-text,pop-a,pop-e   <e> text <a>text</a> </e> 	"e": { "#text": "text", "a": "text" }

% State is a stack of dicts.
% each dict is a list of key-value or key-listofvalues
% on push we add a key to the context or a value to the key
% if it exists. 

% initial impl copied from hty_xml_spaf.
format(Evt, q0) ->
    format(Evt, {q0, []});
format(Evt, {State, Stack}) ->
	case Evt of 
		{push, Key} ->
		    PerhapsListStart = case Key of
			            <<$@, _/binary>> -> <<"">>;
			            _ -> <<"[">>
			end,
			PerhapsComma = case State of
			    q0 -> <<"">>;
			    no_comma -> <<"">>;
			    yes_comma -> <<",">>
			end,
			Output = <<PerhapsComma/binary, 
			           ${, $",Key/binary,$",$:,
			           PerhapsListStart/binary>>,
		    {ok, {no_comma, [{State, Key}|Stack]}, Output};
 		{text, Text} ->
		    case State of
		        no_comma ->
		            {ok, {yes_comma, Stack}, <<$", Text/binary, $">>};
		        yes_comma -> 
		            {ok, {yes_comma, Stack}, <<$,, $", Text/binary, $">>}
		    end;
		{pop, Key} ->
		    case Stack of
                [] ->
			        {err, badpop, []};
			    [{_Restate, Key}|Rest] ->
			        IsAttr = case Key of
			            <<$@, _/binary>> -> true;
			            _ -> false
			        end,
			        case IsAttr of 
			            true ->
			                case State of
			                    yes_comma ->
			                        {ok, {yes_comma, Rest}, <<$}>>};
			                    no_comma ->
			                        {ok, {yes_comma, Rest}, <<"null}">>}
			                end;
 		                false ->
 		                    {ok, {yes_comma, Rest}, <<$], $}>>}
                    end     
			end;
		{kv, <<Key/binary>>, Value} ->
		    {ok, {kv, Stack}, <<"kv", Key/binary, Value/binary>>}
	end.