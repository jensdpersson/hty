%% @author jens
%% @doc @todo Add description to hty_assign_resource.


-module(hty_assign_resource, [Assignments, Subs]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Htx) ->
	Htx1 = lists:foldl(fun({Key, Value}, Acc) ->
						Acc:bind(Key, Value)
				end, Htx, Assignments),
	Htx1:dispatch(Subs).


%% ====================================================================
%% Internal functions
%% ====================================================================


