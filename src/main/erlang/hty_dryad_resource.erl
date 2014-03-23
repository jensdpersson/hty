%% @author jens
%% @doc @todo Add description to hty_dryad_resource.


-module(hty_dryad_resource, [Storage, Taxonomy, Roottype]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Htx) ->
	case locate(Htx:path_below()) of
		root ->
			pass;
		_ ->
			no
	end.
	
%	case  of
%		[] ->
%			case Htx:method() of
%				'GET' ->
					
			

%% ====================================================================
%% Internal functions
%% ====================================================================

% We need to know for any sub whether it is a leaf.
% On folders, we can add new subs according to rules
% in the folder type. On leaves we cannot add subs but post
% will create a new revision of the leaf.
% This type info cannot be encoded into segments from client
% else the whim of the client would be able to retype and perform
% edits on the data that the type would otherwise hinder.
% Further, if we then rewrite segments upon putting stuff in storage
% we get a remapping task that can cause lots of queries to storage to resolve
% a single client url. Also, at this level we want to support some MKCOL 
% like functianlity but we cannot enforce that in the storage contract.
% To solve typing info requirement while creating collections in storage still
% using only GET and PUT we then put the info about a folder, such as type
% in an index.xml data file. Storing a collection in storage amounts to PUTting
% the corresponding .../$collection/index.xml as the storage contract is expected
% to autocreate parent dirs. 
% Possibly, we could list the contents of a folder inside that index.xml 
% (for the price of extra storage requests per file addition/removal) and
% use versioning on the folder content through that.
locate(_Path) ->
	notyet.