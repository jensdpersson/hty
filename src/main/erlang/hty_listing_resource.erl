%% Author: jens
%% Created: 23 mar 2013
%% Description: TODO: Add description to hty_listing_resource
-module(hty_listing_resource, [Filename, Fspath]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/1]).

%%
%% API Functions
%%
handle(Htx) ->
	case Htx:path_below() of
		[] ->
			list_on_get(Htx);
		[Filename] ->
			list_on_get(Htx);
		_ ->
			io:format("Declining INDEX on ~p~n", [Htx:path_below()]),
			Htx:not_found()
	end.	

%%
%% Local Functions
%%
list_on_get(Htx) ->
	case Htx:method() of
		'GET' ->
			list(Htx);	
		_ ->
			Htx:method_not_allowed(['GET'])
	end.

	
list(Htx) ->
	Htx1 = Htx:rsp_header("Content-Type", "application/xml"),
	SpafEvts = [{pop, <<"dir">>}],
	SpafEvts1 = add_files(Fspath:list(), SpafEvts),
	SpafEvts2 = [{push, <<"dir">>}|SpafEvts1],
	{Htx2, _} = lists:foldl(
								fun(Evt, {Htxn, Qn}) ->
										 io:format("Evt, Qn [~p, ~p]~n", [Evt, Qn]),
										 {ok, Qn1, Bin} = hty_xml_spaf:format(Evt, Qn),
										 io:format("Bin, ~p~n", [Bin]),
										 Htxn1 = Htxn:echo(Bin), 
										 {Htxn1, Qn1}
									;(E, Q) -> io:format("PIM ~p,~p~n", [E,Q]) end, 
								{Htx1, q0},
								SpafEvts2),
	io:format("LIM!"),
	Htx2:ok().

add_files([In|Ins], Outs) ->
	Outs1 = [{push, <<"file">>},
					 {text, list_to_binary(In:basename())},
					 {pop, <<"file">>}| Outs],
	add_files(Ins, Outs1);
add_files([], Outs) ->
	Outs.	
	
