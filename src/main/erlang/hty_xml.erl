%% Author: jens
-module(hty_xml).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([format/1]).

%%
%% API Functions
%%
format(Node) ->
	case Node of
		{[Letter|Letters], Subs} ->
			case Letter of
				$@ ->
					format_attr(Letters, hd(Subs));
				$? -> 
					format_pi(Letters, hd(Subs));
				_ ->
					format_elm([Letter|Letters], Subs)
			end;
		_ ->
			format_value(Node)
	end.

%%
%% Local Functions
%%
format_attr(Name, Value) ->
	[Name, $=, $", Value, $"].

format_pi(Name, Value) ->
	["<?", Name, " ", Value, "?>"].

format_elm(Name, Subs) ->
	case Subs of
		[] ->
			[$<, Name, "/>"];
		_ ->
			IsAttr = fun({[$@|_], _}) -> true; (_) -> false end,
			{Attrs, Children} = lists:partition(IsAttr, Subs),
			[$<, 
			 Name,
			 " ", 
			 lists:map(fun format/1, Attrs),
			 ">",			 
			 lists:map(fun format/1, Children),
			 "</",
			 Name,
			 ">"	
			 ]
	end.

format_value(Node) -> Node.
		 
		 
		 