%% Author: jens
%% Created: 1 mar 2013
%% Description: TODO: Add description to hty_status_resource
-module(hty_status2_resource, [Fspath, Content]).

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
    Htx1 = Htx:dispatch([Content]),
    {Status, _} = Htx1:status(),
    IsStatusFileFor = fun(Fs) ->
			      Prefix = Fs:prefix(),
			      case string:tokens(Prefix, "_") of
				  ["status", Code] ->
				      io:format("Status ~p =? ~p~n", [Code, Status]),
				      case list_to_integer(Code) of
					  Status ->
					      true;
					  _ ->
					      false
				      end;
				  _ ->
				      false
			      end
		      end,
    Statusfiles = Fspath:list(IsStatusFileFor),
    io:format("Statusfiles ~p~n", [Statusfiles]),
    case Statusfiles of
	[Statusfile] ->
	    Htx2 = Htx1:ok(),
	    %Htx3 = Htx2:clear(),
	    Htx2:sendfile(Statusfile:filepath());
	[] -> Htx1
    end.


%%
%% Local Functions
%%

