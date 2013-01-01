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
	Content = fun(Fspath1) ->
					  case Fspath1:parts() of
						  ["content"|_] -> 
							  true;
						  _ -> false
					  end
			  end,
	case Fspath:parts() of
		[Rules, "rules"] ->
			case change_rules(Rules, Allrules) of
				{ok, Rules2} ->
					case Fspath:walk(Rules2, Content) of
						[{ok, Resource, _, _}] ->
							{claim, Resource};
						Que ->
							{block, {badwalk, Que}}
					end;		
				{no, _} ->
					{block, badrules}
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

