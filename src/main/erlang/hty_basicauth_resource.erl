%% Author: jens
%% Created: 22 jan 2013
%% Description: TODO: Add description to basic_auth_resource
-module(hty_basicauth_resource).

%%
%% Exported Functions
%%
-export([handle/2, new/1]).

-record(hty_basicauth_resource, {subs}).

%%
%% API Functions
%%
new(Subs) ->
  #hty_basicauth_resource{subs=Subs}.

handle(Htx, This) ->
  case Htx:req_header('Authorization') of
    [] ->
      challenge(Htx);
    [<<"Basic ", Auth/binary>>] ->
      check(Htx, Auth, This);
    _ ->
      challenge(Htx)
  end.

%%
%% Local Functions
%%
challenge(Htx) ->
    Realm = Htx:realm(),
    Htx1 = Htx:rsp_header('WWW-Authenticate',  [<<"Basic realm=\"">>, Realm:name(), "\""]),
    Htx1:status(401, "Not Authorized").

check(Htx, Auth, This) ->
    String = base64:mime_decode(Auth),
    {Username, <<$:, Password/binary>>} = hty_scan:until(String, $:),
    Realm = Htx:realm(),
    case Realm:auth(Username, Password) of
	{ok, Principal} ->
	    Htx1 = Htx:principal(Principal),
	    Htx1:dispatch(This#hty_basicauth_resource.subs);
	no ->
	    challenge(Htx)
    end.
