-module(hty_proc).
-export([mount/4, start/0, stop/0, list/1, find/2]).

mount(Baseurl, Namespace, JobId, Resource) ->
    ?MODULE ! {submit, Namespace, JobId, Resource, self()},
    receive
        ok -> Baseurl ++ "/" ++ Namespace ++ "/" ++ JobId
    end.

start() -> spawn(fun() -> loop([]) end).

stop() -> 
    ?MODULE ! {stop, self()},
    receive
        {ok, stopped} -> {ok, stopped}
    end.

list(Namespace) ->
    ?MODULE ! {list, Namespace, self()},
    receive
        {ok, List} -> {ok, List}
    end.

find(Namespace, JobId) ->
    ?MODULE ! {find, Namespace, fun(Item) ->
        case Item of
            {JobId, _} -> true;
            _ -> false
        end
    end, self()},
    receive
        {ok, {_JobId, Resource}} -> {ok, Resource};
        no -> no
    end.

loop(Namespaces) ->
    receive 
        {submit, Namespace, JobId, Resource, ReplyTo} ->
            ReplyTo ! ok,
            loop(hty_util:keyappend({JobId, Resource}, Namespace, Namespaces));
        {stop, ReplyTo} ->
            ReplyTo ! {ok, stopped};
        {find, Namespace, Finder, ReplyTo} ->
            case lists:keyfind(Namespace, 1, Namespaces) of
                false -> ReplyTo ! no;
                {_, Resources} ->
                    case lists:find(Finder, Resources) of % TODO find is not a function the that module!
                        false -> ReplyTo ! no;
                        {value, Value} -> ReplyTo ! {ok, Value}
                    end
            end,
            loop(Namespaces);
        {list, Namespace, ReplyTo} ->
            case lists:keyfind(Namespace, 1, Namespaces) of
                false -> ReplyTo ! no;
                {_, Resources} ->
                    JobIds = lists:map(fun({JobId,_Res}) -> 
                        JobId 
                    end, Resources),
                    ReplyTo ! {ok, JobIds}
            end,
            loop(Namespaces)
    end.
    

    
    
    
    

    