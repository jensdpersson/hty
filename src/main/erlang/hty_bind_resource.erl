%% @author jens
%% @doc @todo Add description to hty_bind_resource.


-module(hty_bind_resource).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/3, handle/2, next/2]).

-record(hty_bind_resource, {key, value, next}).

new(Key, Value, Next) ->
    #hty_bind_resource{key=Key, value=Value, next=Next}.

next(NewNext, This) ->
	This#hty_bind_resource{next=NewNext}.

handle(Htx, This) ->
	Htx1 = Htx:bind(This#hty_bind_resource.key, This#hty_bind_resource.value),
	Htx1:dispatch(This#hty_bind_resource.next).

%% ====================================================================
%% Internal functions
%% ====================================================================


