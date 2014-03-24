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
-export([list/2, list/3]).

%%
%% API Functions
%%

list(Htx, Fspath) ->
	list(Htx, Fspath, []).

list(Htx, Fspath, Attrs) ->
	case Htx:req_header('Accept') of
		[<<"text/uri-list">>] ->
			listUrilist(Htx, Fspath);
		Other ->
			io:format("Got non-urilist accept header ~p~n", [Other]),
			listXml(Htx, Fspath, Attrs)
	end.

listUrilist(Htx, Fspath) ->
	Htx1 = lists:foldl(fun(Item, Acc) -> 
					Acc:echo([Item:basename(), 13, 10])
				end, Htx, Fspath:list()),
	Htx2 = Htx1:rsp_header("Content-Type", "text/uri-list"),
	Htx2:ok().
 
listXml(Htx, Fspath, Attrs) ->
	Htx1 = Htx:rsp_header("Content-Type", "application/xml"),
	SpafEvts = [{pop, <<"dir">>}],
	SpafEvts1 = add_files(Fspath:list(), SpafEvts, Attrs),
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
add_files([In|Ins], Outs, Attrs) ->
	Outs1 = [{text, list_to_binary(In:basename())},
			 {pop, <<"file">>}|Outs],
	Outs2 = lists:foldl(fun(Attr, Acc) -> add_attribute(In, Attr) ++ Acc end, Outs1, Attrs), 
	Outs3 = [{push, <<"file">>}|Outs2],
	add_files(Ins, Outs3, Attrs);
add_files([], Outs, _) ->
	Outs.	

add_attribute(Fspath, last_modified) ->
	[{kv, <<"@t">>, list_to_binary(Fspath:last_modified())}];
add_attribute(_, _) ->
	[].


