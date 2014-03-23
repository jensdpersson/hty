%% @author jens
%% @doc @todo Add description to hty_aggregate_resource.


-module(hty_aggregate_resource, [Tag, Subs]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Htx) ->
	case Htx:path_below() of
		[] -> 
			case Htx:method() of
				'GET' ->
					Htx1 = Htx:out([$<, Tag, $>]),
					Htx2 = lists:foldl(
							 fun(Sub, Acc) ->
									 H0 = hty_tx_factory:new(),
									 H1 = H0:method('GET'),
									 H2 = H1:dispatch(Sub),
									 case H2:status() of
										 {200, _} ->
											 Acc:copy(H2);
										 _ ->
											 io:format("Aggregate fail on [~p]~n",[Sub])
									 end
							 end,
							 Htx1,
							 Subs),
					Htx2:out([$<, $/, Tag, $>]);
				_ ->
					Htx:dispatch(Subs)
			end;
		_ ->
			Htx:dispatch(Subs)
	end.
			

%% ====================================================================
%% Internal functions
%% ====================================================================


