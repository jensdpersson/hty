-module(hty_listdir_resource).
-record(hty_listdir_resource, {key}).
-export([mount/1, handle/2]).

mount(Fspath) ->
  case lists:reverse(Fspath:parts()) of
    ["listdir", StorageKey|_] ->
      {ok, #hty_listdir_resource{key=StorageKey}};
    _ ->
      {error, "listdir resource needs storage key"}
  end.


handle(Htx, This) ->
    case Htx:method() of
        'GET' -> do_get(Htx, This);
        Other -> Htx:method_not_allowed(["GET"])
    end.
    
do_get(Htx, This) ->
  Key = This#hty_listdir_resource.key,
  case hty_pathmapper:htx_to_fspath(Htx, Key) of
    {ok, Fspath} ->
      case Htx:req_header('Accept') of
        [<<"text/uri-list">>] ->
          listUrilist(Htx, Fspath);
        Other ->
          io:format("Got non-urilist accept header ~p~n", [Other]),
          listXml(Htx, Fspath, [])
      end;
    {no, Error} ->
      Htx:server_error(Error)
  end.

listUrilist(Htx, Fspath) ->
	Htx1 = lists:foldl(fun(Item, Acc) ->
          PerhapsSlash = case Item:isdir() of
            true -> $/;
            false -> ""
          end,
          Percented = hty_percentencoding:encode(Item:basename()),
					Acc:echo([Percented, PerhapsSlash, 13, 10])
				end, Htx, Fspath:list()),
	Htx2 = Htx1:rsp_header("Content-Type", "text/uri-list"),
	Htx3 = Htx2:ok(),
  Htx3:commit().

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
	Htx3 = Htx2:ok(),
  Htx3:commit().

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
