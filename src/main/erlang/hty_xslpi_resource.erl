%% Author: jens
%% Created: 28 jan 2013
%% Description: TODO: Add description to hty_xslpi_resource
-module(hty_xslpi_resource, [Xslpi, Subs]).

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
	XslURL = hty_percentencoding:decode(list_to_binary(Xslpi)), 
	Htx1 = Htx:echo([<<"<?xml-stylesheet type=\"text/xsl\" href=\"">>,
									  XslURL, <<".xsl\"?>">>]),
	Htx1:dispatch(Subs).
%%
%% Local Functions
%%

