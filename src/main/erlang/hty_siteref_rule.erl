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
-export([match/2]).

%%
%% API Functions
%%
match(Fspath, _Rules) ->
    case Fspath:parts() of
		[Siteref, siteref] ->
	    	%% egentligen, kolla i Siteref.siteref som en fil efter alias.
	    	{claim, {resource, hty_siteref_resource:new(Siteref)}};
		_ ->
			next
    end.

%%
%% Local Functions
%%

