%% Author: jens
%% Created: 25 jul 2012
%% Description: TODO: Add description to trema
-module(trema).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([new/0, insert/3]).

%%
%% API Functions
%%

new() -> [].

insert(T, P, V) -> 
	insert([], T, P, V).

lookup(P, T) -> no.
	

%%
%% Local Functions
%%

wrapup([], Tz) -> Tz;
wrapup([T|Ta], Tz) -> 
	wrapup(Ta, [T|Tz]).

%replacement case
insert(Ta, [{K, {Vo, To}}|Tz], [K], V) ->
	wrapup(Ta, [K, {V, To}|Tz]);
insert(Ta, [{K, {Vo, To}}|Tz], [K|Ks], V) ->
	


insert(Ta, [{K, V1, T1}|Tz], [K], V) ->
	insert()
insert(Ta, [{K,V}])
