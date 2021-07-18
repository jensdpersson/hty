%% Author: jens
%% Created: 27 feb 2013
%% Description: TODO: Add description to hty_fsrealm
-module(hty_fs_realm).

-export([mount/2, new/2, name/1, signup/3, auth/3]).

-record(hty_fs_realm, {realmname, fspath}).

mount(Fspath, _Mc) ->
    case lists:reverse(hty_fspath:parts(Fspath)) of
			[_Ext, Name | _] ->
	    	{ok, new(Name, Fspath)};
			_ ->
	    	{ok, new("defaultrealm", Fspath)}
    end.

new(Realmname, Fspath) ->
	#hty_fs_realm{realmname=Realmname, fspath=Fspath}.

name(This) -> This#hty_fs_realm.realmname.

-spec signup(string(), string(), any()) ->
				ok | {no, badnick} | {no, exists} | {no, createerror}.
signup(Nick, Pass, This) ->
    Fspath = This#hty_fs_realm.fspath,
    case validate(Nick) of
	no ->
	    {no, badnick};
	ok ->
	    Userdir = hty_fspath:subpath(["data", Nick], Fspath),
	    case hty_fspath:exists(Userdir) of
			true ->
		    	{no, exists};
			false ->
		    	case hty_fspath:mkdir(Userdir) of
					{error, _Error} ->
			    		{no, createerror};
					ok ->
			    Secretfile = hty_fspath:subpath(["secret"], Userdir),
			    Secret = crypto:hash(md5, Pass),
			    case hty_fspath:save([Secret, 10], Secretfile) of
						{error, _Error} ->
							{no, createerror};
						ok ->
				    	ok
			    end
		    end
	    end
    end.

auth(Nick, Pass, This) ->
    Fspath = This#hty_fs_realm.fspath,
    case validate(Nick) of
	ok ->
	    Secretfile = hty_fspath:subpath(["data", binary_to_list(Nick), "secret"], Fspath),
	    case hty_fspath:load(Secretfile) of
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
