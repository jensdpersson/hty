-module(hty_override_resource).


-export([handle/2, new/2]).
-record(hty_override_resource, {filename, folder}).

new(Filename, Folder) ->
    #hty_override_resource{filename=Filename, folder=Folder}.

handle(Htx, This) ->
    Filename = This#hty_override_resource.filename,
    Below = hty_tx:path_below(Htx),
    case lists:reverse(Below) of
        [Filename|_] ->
            case hty_tx:method(Htx) of
                'GET' ->
                    ascend(Htx, Below, This);
                 _ ->
                    hty_tx:method_not_allowed(['GET'], Htx)
            end;
         _ ->
            hty_tx:not_found(Htx)
    end.

ascend(Htx, Path, This) ->
    Filename = This#hty_override_resource.filename,
    Folder = This#hty_override_resource.folder,
    Subfolder = hty_fspath:subpath(Path, Folder),
    File = hty_fspath:subpath([Filename], Subfolder),
    case hty_fspath:exists(File) of
	true ->
	    hty_tx:sendfile(hty_fspath:path(File), Htx);
	false ->
	    case lists:reverse(Path) of
		[] -> hty_tx:not_found(Htx);
		[_|Ps] ->
		    ascend(Htx, lists:reverse(Ps), This)
	    end
    end.
