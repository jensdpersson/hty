-module(hty_proc).


submit(Namespace, Resource) ->
    MODULE? ! {submit, Namespace, Resource}.

list(Procspace, Filter) -> notyet.



loop(Namespaces) ->
    receive 
        {submit, Namespace, Resource} ->
            