-module(hty_links_resource).
-record(hty_links_resource, {folder, target}).
-export([handle/2, new/2]).

new(Folder, Target) ->
    #hty_links_resource{folder=Folder, target=Target}.

handle(Htx, This) ->
    Folder = This#hty_links_resource.folder,
    Target = This#hty_links_resource.target,
    case Htx:path_below() of
	[Current|_] ->
	    File = Folder:subpath([Current]),
	    case File:exists() of
		true ->
		    Htx:dispatch(Target);
		false ->
		    Htx:not_found()
	    end;
	[] ->
	    case Htx:method() of
		'GET' ->
		    hty_listing:list(Htx, Folder);
		'POST' ->
		    FormSchema = [{<<"name">>, [], []}], 
		    Htx:recvform(
		      FormSchema, 
		      fun(Form, Htx2) -> 
			      {[], Names, []} = lists:keyfind(<<"name">>, 1, Form),
			      case Names of
				  [Name] ->
				      File = Folder:subpath([Name]),
				      case File:exists() of
					  true ->
					      Htx2:ok();
					  false ->
					      Htx2:created()
				      end;
				  _ ->
				      Htx:bad_request()
			      end
		      end)
	    end
    end.	



