%% Author: jens
%% Created: 5 dec 2012
%% Description: TODO: Add description to hty_filter_rule
-module(hty_filter_rule).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([match/2]).

%%
%% API Functions
%%
match(Fspath, Rules) ->
	case Fspath:parts() of
		["filter"] ->
			Walked = Fspath:walk(Rules, fun(X) -> "content" /= hd(X:parts()) end),
			Filters = lists:flatmap(
						  fun(Item) ->
								  case Item of
									  {ok, Resource, _, _} ->
										  [Resource];
									  _ ->
										  []
								  end
						  end, Walked),
			case Fspath:walk(Rules, fun(X) -> "content" == hd(X:parts()) end) of
				[{ok,Resource,_,_}] -> 
					io:format("Resource:~p~n", [Resource]),
					Rv = hty_filter_resource:new(Filters, Resource),
					{claim, {resource, Rv}};
                [] ->
					block;
				_ ->
					block
			end;	
		_ ->
			next
	end.


%%
%% Local Functions
%%

