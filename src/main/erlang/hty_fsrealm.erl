%% Author: jens
%% Created: 27 feb 2013
%% Description: TODO: Add description to hty_fsrealm
-module(hty_fsrealm).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([new/2, name/1, signup/3, auth/3]).

-record(hty_fsrealm, {realmname, fspath}).


%%
%% API Functions
%%

new(Realmname, Fspath) -> #hty_fsrealm{realmname=Realmname, fspath=Fspath}.

name(This) -> This#hty_fsrealm.realmname.

-spec signup(string(), string(), any()) -> 
				ok | {no, badnick} | {no, exists} | {no, createerror}.
signup(Nick, Pass, This) ->
    Fspath = This#hty_fsrealm.fspath,
    case validate(Nick) of
	no ->
	    {no, badnick};
	ok ->
	    Userdir = Fspath:subpath(["data", Nick]),
	    case Userdir:exists() of
		true ->
		    {no, exists};
		false ->
		    case Userdir:mkdir() of
			{error, _Error} ->
			    {no, createerror};
			ok ->
			    Secretfile = Userdir:subpath(["secret"]),
			    Secret = crypto:hash(md5, Pass),
			    case file:write_file(Secretfile:filepath(), [Secret, 10]) of
				{error, _Error} ->
						%TODO log inconsistent user
				    {no, createerror};
				ok ->
				    ok
			    end
		    end
	    end
    end.

auth(Nick, Pass, This) ->
    Fspath = This#hty_fsrealm.fspath,
    case validate(Nick) of 
	ok ->
	    Secretfile = Fspath:subpath(["data", binary_to_list(Nick), "secret"]),
	    case file:read_file(Secretfile:filepath()) of
		{ok, Binary} -> 
		    Md5 = crypto:hash(md5, Pass),
		    case <<Md5/binary, 10>> of
			Binary ->
			    {ok, {Nick, [Nick]}};
			_ ->
			    no
		    end;
		{error, _Error} ->
		    no
	    end;
	no ->
	    no
    end.

%%
%% Local Functions
%%
validate(Nick) when is_binary(Nick) ->
	validate(binary_to_list(Nick));
validate([]) -> 
	no;
validate([N|ICK]) ->
	case ($a =< N) and ($z >= N) of
		true ->
			validate2(ICK);
		false ->
			no
	end.

validate2([]) ->
	ok;
validate2([X|Xs]) ->
	case (($a =< X) and ($z >= X)) or 
        (($0 =< X) and ($9 >= X)) of
		true ->
			validate2(Xs);
		false ->
			no
	end.
