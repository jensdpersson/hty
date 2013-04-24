%% Author: jens
%% Created: 17 apr 2013
%% Description: TODO: Add description to hty_listing
-module(hty_listing).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([list/2]).

%%
%% API Functions
%%

list(Htx, Fspath) ->
	Htx1 = Htx:rsp_header("Content-Type", "application/xml"),
	SpafEvts = [{pop, <<"dir">>}],
	SpafEvts1 = add_files(Fspath:list(), SpafEvts),
	SpafEvts2 = [{push, <<"dir">>}|SpafEvts1],
	{Htx2, _} = lists:foldl(
								fun(Evt, {Htxn, Qn}) ->
										 {ok, Qn1, Bin} = hty_xml_spaf:format(Evt, Qn),
										 Htxn1 = Htxn:echo(Bin), 
										 {Htxn1, Qn1}
									;(E, Q) -> io:format("BADCASE:PIM ~p,~p~n", [E,Q]) end, 
								{Htx1, q0},
								SpafEvts2),
	Htx2:ok().


%%
%% Local Functions
%%
add_files([In|Ins], Outs) ->
	Outs1 = [{push, <<"file">>},
					 {text, list_to_binary(In:basename())},
					 {pop, <<"file">>}| Outs],
	add_files(Ins, Outs1);
add_files([], Outs) ->
	Outs.	

