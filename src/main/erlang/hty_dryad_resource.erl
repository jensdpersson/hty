%% @author jens
%% @doc @todo Add description to hty_dryad_resource.


-module(hty_dryad_resource, [StorageKey, TaxonomyKey, RoottypeKey]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Htx) ->
	Isdir = Htx:path_final_slash(),
	case Htx:method() of
		'GET' -> 
			case Isdir of
				true ->
					listdir(Htx);
				false ->
					serve(Htx)
			end;
		'POST' ->
			case Isdir of
				true ->
					append(Htx);
				false ->
					derive(Htx)
			end;
		_ ->
			Htx:method_not_allowed(['GET', 'POST'])
	end.

% Mapping: 
% GET $path -> GET $path
% POST $folder name=$name&type=folder -> PUT $folder/$name.$type/.mkcol.xml 
% but these are wrapped in Storage and perhaps Taxonomy operations.

%We assume that last segment $name.$ext is a leaf.

%POST på folder ska skapa sub, men vi måste veta typ. 
%Subfil och subfolder skapas på
%olika sätt.

%POST på fil uppdaterar innehåll
%GET på folder listar subs. 
%Urilist GET på fil listar revisioner?
%om vi ska hantera revisioner på det sättet?

%Vad ska vara standard för en Storage att hantera GET på en folder?

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

listdir(Htx) ->
	Path2 = Htx:path_below() ++ ["master.urilist"],
	MasterHtx = Storage:get(Path2),
	case MasterHtx:status() of
		{200, _} ->
			(Htx:copy(MasterHtx)):ok();
		Status -> 
			io:format("Error fetching ~p from storage~n", [Path2]),
			Htx:status(Status)
	end.
		
append(Htx) ->
	% Create file or folder?
	FormSchema = [{<<"name">>, [], []}, 
				  {<<"type">>, [], []}, 
				  {<<"isleaf">>, [], []},],
	Onform = fun(Form) ->
					 Name = lists:keyfind(<<"name">>, 1, Form),
					 Type = lists:keyfind(<<"type">>, 1, Form),
					 case lists:keyfind(<<"isleaf">>, 1, Form) of 
						 [<<"true">>] ->
							 create_doc(Htx, Name, Type);
						 _ ->
							 create_col(Htx, Name, Type)
					 end
			 end,
	Htx:recvform(FormSchema, Onform).
	
	% Get zero-value for type from taxo
	% or empty urilist. PUT on Storage.
create_doc(Htx, Name, Type) ->
	Zero = zero(Type),
	case Zero:status() of
		{200, _} ->
			Put
	Path2 = Htx:path_below() ++ ["master.urilist"],
	Store = Htx:bound(StorageKey),
	Store:put(Path2, ),

