%% @author jens
%% @doc @todo Add description to hty_assign_resource.


-module(hty_assign_resource).
-record(hty_assign_resource, {assignments, subs}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/2, new/2]).

handle(Htx, This) ->
    {_, Assignments, Subs} = This,
    Htx1 = lists:foldl(fun({Key, Value}, Acc) ->
			       Acc:bind(Key, Value)
		       end, Htx, Assignments),
    Htx1:dispatch(Subs).

new(Assignments, Subs) ->
    #hty_assign_resource{assignments=Assignments, subs=Subs}.

%% ====================================================================
%% Internal functions
%% ====================================================================


