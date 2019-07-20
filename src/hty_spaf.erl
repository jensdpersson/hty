%% Author: jens
%% Created: 16 jan 2013
%% Description: TODO: Add description to hty_spaf
-module(hty_spaf).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([chain/1, binder/1]).

-type state() :: any().
-type output() :: list().
-type result() :: {ok, state(), output()} | {no, any()}.

-type event() :: {push, binary()} |
									{pop, binary()} |
				 {kv, binary(), binary()} |
				  {text, binary()}.

-export_type([state/0, result/0]).
-export_type([event/0]).
%%
%% API Functions
%%
chain([]) ->
	fun(Bin, Q) ->
			 {ok, Q, [Bin]}
	end;
chain([Parser, Formatter]) ->
	fun(Bin, Q) -> 
			 io:format("invoking chain(~p,~p)~n", [Bin, Q]),
			 [Qp, Qf] = case Q of
										q0 -> [q0, q0];
										Other -> Other
									end,
			 case Parser(Bin, Qp) of
				 {ok, Qp1, Evts} ->
					 Fold = fun(Item, {Qn, Outs}) ->
											 io:format("SPAF(~p)~n", [Item]),
											 case Formatter(Item, Qn) of
												 {ok, Qn1, Out1} -> {next, {Qn1, lists:reverse(Out1) ++ Outs}};
												 {no, Reason} -> {break, {Item, Reason}}
											 end 
									end,
					 case hty_util:fold(Fold,	 {Qf, []}, Evts) of
						 {nobreak, {Qf1, Outs}} ->
							 {ok, [Qp1, Qf1], lists:reverse(Outs)};
						 {break, Reason} ->
							 {no, Reason}
					 end;
				 {no, Reason} -> 
					 {no, Reason}
			 end
	end.
%chain(Fs) ->
%	fun(P, Q) -> 
%		Q1 = case Q of
%					 q0 ->
%						 lists:map(fun(F) -> {F, q0} end, Fs);
%					 _ ->
%						 Q
%				 end,
%		chain2(P, Q1, [])
%	end.
%
%chain2

binder(Schema) ->
	fun(E, Q) ->
			 Bindings = case Q of
							q0 -> Schema;
							_ -> Q 
						end,
			 io:format("spaf binder got ~p~n", [E]),
			 case E of
				 {kv, Key, Value} ->
				   io:format("Bind ~p in ~p~n", [E, Bindings]),
			  	 case bind(Key, Value, [], Bindings) of
						 {ok, Bindings1} ->
							 {ok, Bindings1, []};
						 {no, Error} ->
							 {no, Error}
					 end;
				 eos ->
					 {ok, qf, Bindings};
				 {_Other, _Key} ->
					 {ok, Bindings, []}
			 end
	end.

%%
%% Local Functions
%%
bind(Key, Val, Aboves, [{Key, Old, Constraints}|Bs]) ->
	New = [Val|Old],
	{ok, rewind([{Key, New, Constraints}|Aboves], Bs)};
bind(Key, Val, Aboves, [Other|Bs]) ->
	bind(Key, Val, [Other|Aboves], Bs);
bind(_Key, _Val, Aboves, []) ->
	{ok, lists:reverse(Aboves)}.

rewind([X|Xs], Ys) ->
	rewind(Xs, [X|Ys]);
rewind([], Ys) -> Ys.

%violations([C|Cs], Values) ->
%	case C of
%		{occurs, Min, Max}
