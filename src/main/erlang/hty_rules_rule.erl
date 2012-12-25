%%% Author: jens
%% Created: 4 dec 2012
%% Description: TODO: Add description to hty_rules_rule
-module(hty_rules_rule).

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

match(Fspath, Allrules) ->
	case Fspath:parts() of
		[Rules, "rules"] ->
			case change_rules(Rules, Allrules) of
				{ok, Rules2} ->
					Fspath:walk(Rules2);
				{no, _} ->
					block
			end;
		_ -> next
	end.
	
change_rules(Prefix, Rules0) ->
	case hty_chgspec:parse(Prefix) of
		{ok, Remove, Add} ->
			Cull = fun(A) -> A end,
			Ctor = fun(A) -> list_to_atom(A ++ "_rule") end,
			Cmp = fun(A,B) -> Ctor(A) =:= B end,
			Vector = hty_vector:new(Cmp, Cull, Ctor),
			{ok, Vector:update(Rules0, Add, Remove)};
		Other ->
			io:format("Rule result for ~p is ~p~n", [Prefix, Other]),
			{no, Rules0}
	end.
%%
%% Local Functions
%%

