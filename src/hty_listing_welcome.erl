%% Author: jens
%% Created: 17 apr 2013
-module(hty_listing_welcome).
-export([list/2, list/3, new/0]).

new() -> hty_listing_welcome.

list(Htx, Fspath) ->
  list(Htx, Fspath, []).

list(Htx, Fspath, Attrs) ->
  case hty_tx:req_header('Accept', Htx) of
    [<<"text/uri-list">>] ->
      listUrilist(Htx, Fspath);
    Other ->
      io:format("Got non-urilist accept header ~p~n", [Other]),
      listXml(Htx, Fspath, Attrs)
  end.

listUrilist(Htx, Fspath) ->
	Htx1 = lists:foldl(fun(Item, Acc) ->
          PerhapsSlash = case hty_fspath:isdir(Item) of
            true -> $/;
            false -> ""
          end,
          Percented = hty_percentencoding:encode(hty_fspath:basename(Item)),
					hty_tx:echo([Percented, PerhapsSlash, 13, 10], Acc)
				end, Htx, hty_fspath:list(Fspath)),
	Htx2 = hty_tx:rsp_header("Content-Type", "text/uri-list", Htx1),
	Htx3 = hty_tx:ok(Htx2),
  hty_tx:commit(Htx3).

listXml(Htx, Fspath, Attrs) ->
  Htx1 = hty_tx:rsp_header("Content-Type", "application/xml", Htx),
  SpafEvts = [{pop, <<"dir">>}],
  SpafEvts1 = add_files(sort_files(hty_fspath:list(Fspath)), SpafEvts, Attrs),
  SpafEvts2 = [{push, <<"dir">>}|SpafEvts1],
  {Htx2, _} = lists:foldl(
								fun(Evt, {Htxn, Qn}) ->
										 {ok, Qn1, Bin} = hty_xml_spaf:format(Evt, Qn),
										 Htxn1 = hty_tx:echo(Bin, Htxn),
										 {Htxn1, Qn1}
									;(E, Q) -> io:format("BADCASE:PIM ~p,~p~n", [E,Q]) end,
								{Htx1, q0},
								SpafEvts2),
	Htx3 = hty_tx:ok(Htx2),
  hty_tx:commit(Htx3).

%%
%% Local Functions
%%

-spec sort_files(Files :: [any()]) -> [any()].
sort_files(Files) -> lists:sort(Files).

add_files([In|Ins], Outs, Attrs) ->
  Outs1 = [{text, list_to_binary(hty_fspath:basename(In))},
    {pop, <<"file">>}|Outs],
  Outs2 = lists:foldl(fun(Attr, Acc) -> add_attribute(In, Attr) ++ Acc end, Outs1, Attrs),
  Outs3 = [{push, <<"file">>}|Outs2],
  add_files(Ins, Outs3, Attrs);
add_files([], Outs, _) ->
  Outs.

add_attribute(Fspath, last_modified) ->
  [{kv, <<"@t">>, list_to_binary(hty_fspath:last_modified(Fspath))}];
add_attribute(_, _) ->
  [].
