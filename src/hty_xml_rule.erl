%% Author: jens
%% Created: 9 mar 2013
%% Description: TODO: Add description to hty_mime_rule
-module(hty_xml_rule).

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
	"xml" ->
	    {claim, {resource, hty_sendfile_resource:new("text/xml", Fspath)}};
	_ ->
	    next
    end.


%%
%% Local Functions
%%