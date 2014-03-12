%% @author jens
%% @doc @todo Add description to hty_aggregate_resource.


-module(hty_aggregate_resource, [Tag, Subs]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Htx) ->
	case Htx:path_below() of
		case Htx:method() of
			'GET' ->
				Htx:out([$<, Tag, $>]),
				lists:foldl(
				  		fun(Sub, Acc) ->
								Htx0 = hty_tx_factory:new(),
								Htx1 = Htx0:method('GET'),
								Htx2 = Htx1:dispatch(Sub),
								case Htx2:status() of
									{200, _} ->
										Acc:copy(Htx2);
									_ ->
										io:format("Aggregate fail on [~p]~n",[Sub]);
								end, 
						Htx,
						Subs 
					),
			

%% ====================================================================
%% Internal functions
%% ====================================================================


