%% Author: jens
%% Created: 22 jan 2013
%% Description: TODO: Add description to basic_auth_resource
-module(hty_basicauth_resource, [Subs]).

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
	io:format("Auth: ~p~n", [Htx:req_header('Authorization')]),
	case Htx:req_header('Authorization') of
		[] -> challenge(Htx);
		[<<"Basic ", Auth/binary>>] -> check(Htx, Auth);
		_ -> challenge(Htx)
	end.


%%
%% Local Functions
%%
challenge(Htx) ->
	Realm = Htx:realm(),
	Htx1 = Htx:rsp_header('WWW-Authenticate',  [<<"Basic realm=\"">>, Realm:name(), "\""]),
	Htx1:status(401, "Not Authorized").

check(Htx, Auth) ->
	String = base64:mime_decode(Auth),
	{Username, <<$:, Password/binary>>} = hty_util:until(String, $:),
	Realm = Htx:realm(),
	case Realm:auth(Username, Password) of
		{ok, Principal} -> 
			Htx1 = Htx:loggedin(Principal),
			Htx1:dispatch(Subs);
		no -> Htx:forbidden()
	end.
													
													