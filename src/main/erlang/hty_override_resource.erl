-module(hty_override_resource, [Filename, Folder]).


-export([handle/1]).

handle(Htx) ->
    Below = Htx:path_below(),
    case lists:reverse(Below) of
        [Filename|_] ->
            case Htx:method() of
                'GET' ->
                    ascend(Htx, Below);
                 _ ->
                    Htx:method_not_allowed(['GET'])
            end;
         _ ->
            Htx:not_found()
    end.

ascend(Htx, Path) ->
      Subfolder = Folder:subpath(Path),
      File = Subfolder:subpath([Filename]),
      case File:exists() of
           true ->
               Htx:sendfile(File:filepath());
           false ->
               case lists:reverse(Path) of
                    [] -> Htx:not_found();
                    [_|Ps] ->    
                         ascend(Htx, lists:reverse(Ps))
               end
      end. 