%% @author jens
%% @doc @todo Add description to hty_aggregate_resource.
-module(hty_aggregate_resource).
-record(hty_aggregate_resource, {tag, subs}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/2, new/2]).

new(Tag, Subs) ->
    #hty_aggregate_resource{tag=Tag,subs=Subs}.

handle(Htx, This) ->
    {_, Tag, Subs} = This,
    case Htx:path_below() of
	[] -> 
	    case Htx:method() of
		'GET' ->
		    SubTx = (hty_tx_factory:listdir()):dispatch(Subs),
		    case SubTx:status() of
			{200, _} ->
			    Htx1 = Htx:echo([$<, Tag, $>]),
			    Urilist = hty_urilist:parse_binary(SubTx:rsp_entity()),
			    io:format("Got urilist from subs ~p~n", [Urilist]),
			    {Htx2, _} = lists:foldl(fun copy/2, {Htx1, Subs}, Urilist),
			    Htx3 = Htx2:echo([$<, $/, Tag, $>]),
			    Htx4 = Htx3:rsp_header("Content-Type", "text/xml"),
			    Htx4:ok();
			_ ->
			    Htx:dispatch(Subs)
		    end;
		_ ->
		    Htx:dispatch(Subs)
	    end;
	_ ->
	    Htx:dispatch(Subs)
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

copy(Uri, {Htx, Subs}) ->
    Get = (hty_tx_factory:get(Uri)):dispatch(Subs),
    case Get:status() of
	{200, _} ->
	    {Htx:copy(Get), Subs};
	{307, _} ->
	    case Get:rsp_header("Location") of
		no ->
		    io:format("Redirect without Location from listing ~n"),
		    {Htx, Subs};
		{ok, Location} ->
		    LocationBinary = list_to_binary(Location),
		    LocationPath = hty_uri:parse_path(LocationBinary),
		    case hty_util:fast_forward(Htx:path_above(), LocationPath) of
			{ok, Uri} ->
			    io:format("Cyclical redirect from listing [~p]~n", [Uri]),
			    {Htx, Subs};
			{ok, OtherUri} ->
			    copy(iolist_to_binary(hty_uri:pack(OtherUri)), {Htx, Subs})
		    end
	    end;
	_ ->
	    io:format("Aggregate fail on [~p]~n",[Subs]),
	    {Htx, Subs}
    end.

