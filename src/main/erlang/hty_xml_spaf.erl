%% Author: jens
%% Created: 15 jan 2013
%% Description: TODO: Add description to hty_xml_spaf
-module(hty_xml_spaf).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([format/1, format/2]).

%%
%% API Functions
%%
format(Evt) ->
	Q0 = {not_in_start, []},
	format(Evt, Q0).

format(Evt, {Instart, Stack}) ->
			case Evt of 
				{push, Elm} ->
					Out = case Instart of
									in_start ->
										[$>, $<, Elm];
									not_in_start ->
										[$<, Elm]
								end,
					{ok, {in_start, [Elm|Stack]}, Out};
				{pop, Elm} ->
					case Stack of
						[Elm|Elms] ->
							{ok, {not_in_start, Elms}, close_elm(Instart, Elm)};
						[Other|_Elms] ->
							{no, {badpop, Elm, Other}}
					end;
				{text, Text} ->
					Out = case Instart of
									in_start ->
										[$>, Text];
									not_in_start ->
										[Text]
								end,
					{ok, {not_in_start, Stack}, Out};
				{kv, <<$@, Key/binary>>, Value} ->
					case Instart of
						in_start ->
							{ok, {in_start, Stack}, [" ", Key, $=, $", Value, $"]};
						not_in_start ->
							{no, {badseq, "attrs in body"}}
					end;
				{kv, Key, Value} ->
					Out = [$<, Key, $>, Value, $<, $/, Key, $>],
					Out1 = case Instart of
										 in_start ->
											 [$>|Out];
										 not_in_start ->
											 Out
									 end,
					{ok, {not_instart, Stack}, Out1}
			end.
				
			

%%
%% Local Functions
%%
close_elm(not_in_start, Elm) -> [$<, $/, Elm, $>];
close_elm(in_start, _) -> [$/, $>]. 

					

