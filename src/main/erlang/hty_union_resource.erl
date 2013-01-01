%% Author: jens
%% Created: 31 dec 2012
%% Description: TODO: Add description to hty_union_resource
-module(hty_union_resource, [Resources]).

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
	Fun = fun(Resource) ->
				  Rsp = Resource:handle(Htx),
				  case Rsp:status() of
					  "404 Not Found" -> 
						  no;
					  _ ->
						  {ok, Rsp}
				  end
		  end,
	case hty_util:first_match(Fun, Resources) of
		{ok, Rsp} -> Rsp;
		no -> Htx:not_found()
	end.


%%
%% Local Functions
%%

