%% Author: jens
%% Created: 1 jan 2013
%% Description: TODO: Add description to hty_formdocs_rule
-module(hty_formdocs_rule).

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
match(Fspath, _Rules) ->
	case Fspath:ext() of
		"formdocs" -> 
			[_, Segment|_] = lists:reverse(Fspath:parts()),
			{claim, {resource, hty_formdocs_resource:new(Fspath, Segment)}};
		_ -> next
	end.


%%
%% Local Functions
%%
