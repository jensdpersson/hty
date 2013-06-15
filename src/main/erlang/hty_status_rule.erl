%% Author: jens
%% Created: 1 mar 2013
%% Description: TODO: Add description to hty_status_rule
-module(hty_status_rule).

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
    case Fspath:ext() of
	"status" ->
	    Statusmap = lists:flatmap(fun(File) ->
					      Prefix = File:prefix(),
					      case (catch list_to_integer(Prefix)) of
						  {'EXIT', _} -> 
						      [];
						  Integer ->
						      case File:match(Rules) of
							  {ok, {resource, Resource},_,_} ->
							      [{Integer, Resource}];
							  {no, _, _, _} ->
							      []
						      end
					      end
				      end,
				      Fspath:list()),
	    io:format("Status map ~p~n", [Statusmap]),
	    [{ok, {resource, Content}, _, _}] = Fspath:walk(Rules, fun(Fs) -> case Fs:prefix() of "content" -> true; _ -> false end end),
	    {claim, {resource, hty_status_resource:new(Content, Statusmap)}};
	_ -> next
    end.

%%
%% Local Functions
%%

