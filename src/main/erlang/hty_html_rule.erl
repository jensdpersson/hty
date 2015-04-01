%% Author: jens
%% Created: 9 mar 2013
%% Description: TODO: Add description to hty_mime_rule
-module(hty_html_rule).

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
    case Fspath:ext() of
	"html" ->
	    {claim, {resource, hty_sendfile_resource:new("text/html", Fspath)}};
	_ ->
	    next
    end.


%%
%% Local Functions
%%
