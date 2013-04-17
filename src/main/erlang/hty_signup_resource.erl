%% Author: jens
%% Created: 18 feb 2013
%% Description: TODO: Add description to hty_signup_resource
-module(hty_signup_resource).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/1]).

%%
%% API Functions
%%
handle(Htx) ->
	case Htx:method() of
		'POST' ->
			FormSchema = [
										{<<"nick">>, [], [{occurs, 1, 1}, ascii7]},
										{<<"pass">>, [], [{occurs, 1, 1}, 
																			{len, 6, 30}]}
									 ], 
			Htx:recvform(FormSchema, fun(Form, Htx2) -> onform(Form, Htx2) end);
		_ ->
			Htx:method_not_allowed(['POST'])
	end.
 
%%
%% Local Functions
%%
onform(Form, Htx) ->
	io:format("FORM:~p~n",[Form]),
	{_, [Nick], _} = lists:keyfind(<<"nick">>, 1, Form),
	{_, [Pass], _} = lists:keyfind(<<"pass">>, 1, Form),
	Realm = Htx:realm(),
	case Realm:signup(binary_to_list(Nick), Pass) of
		ok -> {ok, Htx:created()};
		{no, Error} -> {no, Error}
	end.
	