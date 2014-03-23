%% @author jens
%% @doc @todo Add description to hty_bind_resource.


-module(hty_bind_resource, [Key, Value, Next]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1, next/1]).

next(NewNext) ->
	hty_bind_resource:new(Key, Value, NewNext).

handle(Htx) ->
	Htx1 = Htx:bind(Key, Value),
	Htx1:dispatch(Next).

%% ====================================================================
%% Internal functions
%% ====================================================================


