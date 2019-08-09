%% Author: jens
%% Created: 22 jan 2013
%% Description: TODO: Add description to basic_auth_resource
-module(hty_basicauth_resource).

%%
%% Exported Functions
%%
-export([mount/1, handle/2, new/1]).

-record(hty_basicauth_resource, {subs}).


mount(Fspath) ->
  {ok, new(hty_mounter:walk(Fspath, "resource"))}.

new(Subs) ->
  #hty_basicauth_resource{subs=Subs}.

handle(Htx, This) ->
  case hty_tx:req_header('Authorization', Htx) of
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
    Realm = hty_tx:realm(Htx),
    Htx1 = hty_tx:rsp_header('WWW-Authenticate',
                             [<<"Basic realm=\"">>, hty_realm:invoke_name(Realm), "\""],
                             Htx),
    hty_tx:status(401, "Not Authorized", Htx1).

check(Htx, Auth, This) ->
    String = base64:mime_decode(Auth),
    {Username, <<$:, Password/binary>>} = hty_scan:until(String, $:),
    Realm = hty_tx:realm(Htx),
    case hty_realm:invoke_auth(Username, Password, Realm) of
	{ok, Principal} ->
	    Htx1 = hty_tx:principal(Principal, Htx),
	    hty_tx:dispatch(This#hty_basicauth_resource.subs, Htx1);
	no ->
	    challenge(Htx)
    end.
