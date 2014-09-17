-module(hty_override_resource).


-export([handle/2, new/2]).
-record(hty_override_resource, {filename, folder}).

new(Filename, Folder) ->
    #hty_override_resource{filename=Filename, folder=Folder}.

handle(Htx, This) ->
    Filename = This#hty_override_resource.filename,
    Below = Htx:path_below(),
    case lists:reverse(Below) of
        [Filename|_] ->
            case Htx:method() of
                'GET' ->
                    ascend(Htx, Below, This#hty_override_resource.folder);
                 _ ->
                    Htx:method_not_allowed(['GET'])
            end;
         _ ->
            Htx:not_found()
    end.

ascend(Htx, Path, This) ->
    Filename = This#hty_override_resource.filename,
    Folder = This#hty_override_resource.folder,
    Subfolder = Folder:subpath(Path),
    File = Subfolder:subpath([Filename]),
    case File:exists() of
	true ->
	    Htx:sendfile(File:filepath());
	false ->
	    case lists:reverse(Path) of
		[] -> Htx:not_found();
		[_|Ps] ->    
		    ascend(Htx, lists:reverse(Ps), This)
	    end
    end. 
