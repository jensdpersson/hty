%% Author: jens
%% Created: 28 jan 2013
%% Description: TODO: Add description to hty_xslpi_resource
-module(hty_xslpi_resource).
-record(hty_xslpi_resource,  {xslpirules, subs}).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/2]).

%%
%% API Functions
%%

new(XslpiRules, Subs) ->
    #hty_xslpi_resource{xslpirules=XslpiRules, subs=Subs}.

handle(Htx, This) ->
    XslpiRules = This#hty_xslpi_resource.xslpirules,
    Subs = This#hty_xslpi_resource.subs,
    Htx1 = Htx:dispatch(Subs),
    Xchoice = case Htx1:bound("xslpi_choice") of
		  {ok, Xslpichoice} -> Xslpichoice;
		  no -> "any"
	      end,
    case lists:keyfind(Xchoice, 1, XslpiRules) of
	false ->
	    Htx1;
	{_, Url} ->
	    Htx1:prolog(xslpi(Url))
    end.
%%
%% Local Functions
%%
xslpi(XslURL) -> 
    [<<"<?xml-stylesheet type=\"text/xsl\" href=\"">>, 
     hty_percentencoding:decode(list_to_binary(XslURL)),
     <<".xsl\"?>">>].
