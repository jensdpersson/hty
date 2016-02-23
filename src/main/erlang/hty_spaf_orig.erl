%% Author: jens
%% Created: 16 jan 2013
%% Description: TODO: Add description to hty_spaf
-module(hty_spaf_orig).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([chain/1]).

%%
%% API Functions
%%
chain([Parser, Formatter]) ->
	fun(Bin, Q) ->
			 [Qp, Qf] = case Q of
										q0 -> [q0, q0];
										Other -> Other
									end,
			 case Parser:parse(Bin, Qp) of
				 {ok, Qp1, Evts} ->
					 Fold = fun(Item, {Qn, Outs}) ->
											 case Formatter:format(Item, Qn) of
												 {ok, Qn1, Out1} -> {next, {Qn1, [Out1|Outs]}};
												 {no, Reason} -> {break, {Item, Reason}}
											 end
									end,
					 case hty_util:fold(Fold,	 {Qf, []}, Evts) of
						 {nobreak, {Qf1, Outs}} ->
							 {ok, [Qp1, Qf1], lists:reverse(Outs)};
						 {break, Reason} ->
							 {no, Reason}
					 end;
				 {no, Reason} ->
					 {no, Reason}
			 end
	end.

%%
%% Local Functions
%%
