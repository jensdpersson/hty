%% Author: jens
%% Created: 18 aug 2012
%% Description: TODO: Add description to hty_siteref_rule
-module(hty_siteref_rule).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([match/1]).

%%
%% API Functions
%%
match(Walker) ->
  Fspath = Walker:fspath(),
    case Fspath:parts() of
	[Siteref, "siteref"] ->
	    {claim, {resource, hty_siteref_resource:new(Siteref)}};
	_ ->
	    next
    end.

%%
%% Local Functions
%%
