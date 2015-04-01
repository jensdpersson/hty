%% Author: jens
%% Created: 28 jan 2013
%% Description: TODO: Add description to hty_xslpi_rule
-module(hty_xslpi_rule).

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
	case lists:reverse(Fspath:parts()) of
		["xslpi", Xslpath|_] ->
			Subs = Walker:subs(),
                        Xslpaths = parse(Xslpath),
			{claim, {resource, hty_xslpi_resource:new(Xslpaths, Subs)}};
		_ ->
			next
	end.


%%
%% Local Functions
%%
-spec parse(string()) -> [{string(),string()}|string()].
parse(S) ->
    lists:flatmap(fun("") -> [];
                     (Str) ->
                        case string:tokens(Str, "=") of
                            [Value] -> [{"any", Value}];
                            [Key, Value] -> [{Key, Value}]
                        end
              end, string:tokens(S, ",")).
