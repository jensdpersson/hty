%% Author: jens
%% Created: 12 dec 2012
%% Description: TODO: Add description to hty_chgspec
-module(hty_chgspec).
-import(lists, [reverse/1]).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([parse/1]).

%%
%% API Functions
%%
parse(Spec) ->
	s(Spec, [], [], 0).

%%
%% Local Functions
%%
s([$+|S], A, R, I) ->
	item(S, [], A, R, I+1, plus);
s([$-|S], A, R, I) ->
	item(S, [], A, R, I+1, minus);
s([_|S], _A, _R, I) ->
	{no, {nosign, I, S}}.

item([$,|S], [], _, _, I, _Sign) ->
	{no, {emptysymbol, I, S}};
item([$,|S], Symbol, A, R, I, plus) ->
	s(S, [reverse(Symbol)|A], R, I);
item([$,|S], Symbol, A, R, I, minus) ->
	s(S, A, [reverse(Symbol)|R], I);
item([], Symbol, A, R, _I, Sign) ->
	case Sign of
		plus ->  
			{ok, [reverse(Symbol)|A], R};
		minus ->
			{ok, A, [reverse(Symbol)|R]}
	end;
item([S|Ss], Symbol, A, R, I, Sign) ->
	item(Ss, [S|Symbol], A, R, I, Sign).
