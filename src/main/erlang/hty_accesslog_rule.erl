-module(hty_accesslog_rule).

-export([match/2]).

match(Fspath, Rules) ->
    case lists:reverse(Fspath:parts()) of
	["accesslog", Format|_] ->
	    Subpath = Fspath:subpath(["content"]),
	    Subs = Subpath:subs(Rules),
	    Logfolder = Fspath:subpath(["logs"]),
	    {claim, {resource,
				 hty_accesslog_resource:new(Format, Logfolder, Subs)}
      };
	_ ->
	    next
    end.
