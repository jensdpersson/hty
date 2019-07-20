%% Author: jens
-module(hty_formdocs_resource).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/2, new/2]).

-record(hty_formdocs_resource, {fspath, index}).

%%
%% API Functions
%%

new(Fspath, Index) ->
    #hty_formdocs_resource{fspath=Fspath, index=Index}.

handle(Htx, This) ->
    Fspath = This#hty_formdocs_resource.fspath,
    Index = This#hty_formdocs_resource.index,
    case Htx:path_below() of
	[] ->
	    case Htx:method() of
		'GET' ->
		    Htx:dispatch([Index]);
		'POST' ->
		    Htx:method_not_allowed(['GET'])
	    end;
	Segments ->
	    Fspath1 = Fspath:subpath(Segments),
	    case Htx:method() of
		'GET' ->
		    case Fspath1:exists() of
			true ->
        Fspath1:send(Htx);
			false ->
			    Htx:not_found()
		    end;
		'POST' ->
		    Htx1 = Fspath1:recv([fun hty_formtree_spaf:parse/2,
					 fun hty_xml_spaf:format/2], Htx),
		    Htx1:see_other()
	    end
    end.
%%
%% Local Functions
%%
