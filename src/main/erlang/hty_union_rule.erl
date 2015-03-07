%% Author: jens
%% Created: 31 dec 2012
%% Description: TODO: Add description to hty_union_rule
-module(hty_union_rule).

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
		"union" ->
			{claim, {resource, assemble(Fspath, Rules)}};
		_ ->
			next
	end.


%%
%% Local Functions
%%
assemble(Fspath, Rules) ->
	Subs = Fspath:walk(Rules),
	%A list of {ok, Res, Path, Rule}|{no, Reason...}
    %Sort the OKs after prefix Number segment.
	%create a union resource instance
	Cmp = fun compare/2,
	Subs1 = lists:sort(Cmp, Subs),
	Resources = lists:flatmap(fun({ok, {resource, Resource}, _, _}) ->
									  [Resource];
								 (_) -> []
							  end, Subs1),
	hty_union_resource:new(Resources).

extract_position({_,_,X,_}) ->
    X1 = hty_fspath:new(X),
	[X2|_] = X1:parts(),
    list_to_integer(X2).

compare(A, B) ->
	A1 = extract_position(A),
	B1 = extract_position(B),
	A1 < B1.
