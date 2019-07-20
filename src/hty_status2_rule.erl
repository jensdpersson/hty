%% Author: jens
%% Created: 1 mar 2013
%% Description: TODO: Add description to hty_status_rule
-module(hty_status2_rule).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([match/1]).

%%
%% API Functions
%%
match(Walker) ->
  Fspath = Walker:fspath(),
    case Fspath:ext() of
	"status" ->
	    Content = fun(Fspath1) ->
			      case Fspath1:parts() of
				  ["content"|_] ->
				      true;
				  _ -> false
			      end
		      end,
	    case Walker:walk(Content) of
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
