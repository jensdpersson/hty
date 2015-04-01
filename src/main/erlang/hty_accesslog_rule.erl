-module(hty_accesslog_rule).

-export([match/1]).

match(Walker) ->
  Fspath = Walker:fspath(),
      case lists:reverse(Fspath:parts()) of
	["accesslog", Format|_] ->
	    Subpath = Fspath:subpath(["content"]),
	    Subs = (hty_walker:fspath(Subpath)):subs(),
	    Logfolder = Fspath:subpath(["logs"]),
	    {claim, {resource,
				 hty_accesslog_resource:new(Format, Logfolder, Subs)}
      };
	_ ->
	    next
    end.
