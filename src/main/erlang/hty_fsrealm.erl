%% Author: jens
%% Created: 27 feb 2013
%% Description: TODO: Add description to hty_fsrealm
-module(hty_fsrealm, [Realmname, Fspath]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([name/0, signup/2, auth/2]).

%%
%% API Functions
%%
name() -> Realmname.

-spec signup(string(), string()) -> 
				ok | {no, badnick} | {no, exists} | {no, createerror}.
signup(Nick, Pass) ->
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
							Secret = crypto:md5(Pass),
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

auth(Nick, Pass) ->
	case validate(Nick) of 
		ok ->
			Secretfile = Fspath:subpath(["data", binary_to_list(Nick), "secret"]),
		  case file:read_file(Secretfile:filepath()) of
				{ok, Binary} -> 
				Md5 = crypto:md5(Pass),
				case <<Md5/binary, 10>> of
					Binary ->
						{ok, {Nick, [Nick]}};
					_ ->
						no
				end;
				{error, Error} ->
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