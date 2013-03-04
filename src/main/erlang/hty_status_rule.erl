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
			Content = fun(Fspath1) ->
										 case Fspath1:parts() of
											 ["content"|_] -> 
												 true;
											 _ -> false
										 end
								end,
			case Fspath:walk(Rules, Content) of
				[{ok, {resource, Subresource}, _, _}] ->
					Resource = hty_status_resource:new(Fspath, Subresource),
					{claim, {resource, Resource}};
				Que ->
					{block, {badwalk, Que}}
			end;		
		_ -> next
	end.

%%
%% Local Functions
%%

