-module(hty_vhost_resource, [Aliases, Subs]).

-export([handle/1]).

handle(Htx) ->
    case Htx:req_header('Host') of
	[Host] ->
	    case lists:member(Host, Aliases) of
		true ->
		    Htx:dispatch(Subs);
		false ->
		    Htx:not_found()
	    end;
	[] ->
	    Htx:not_found();
	_ ->
	    Htx:bad_request()
    end.

