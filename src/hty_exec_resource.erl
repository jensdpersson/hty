-module(hty_exec_resource).
-record(hty_exec_resource, {subcommander}).
-export([mount/1, handle/2]).

mount(Fspath) ->
  % We need two params: subcommander name and workdir
  {ok, #hty_exec_resource{subcommander=subcommander:start()}}.

handle(Htx, This) ->
    Meth = Htx:method(),
    case Htx:path_below() of
        [] ->
            case Meth of
                'GET' ->
                    list_jobs(Htx);
                'POST' ->
                    create_job(Htx);
                Other ->
                    Htx:method_not_allowed()
            end;
        [Job] ->
            case Meth of
                'GET' ->
                    job_info(Job, Htx);
                'DELETE' ->
                    cancel_job(Job, Htx);
                Other ->
                    Htx:method_not_allowed()
            end;
        [Job, Resource] ->
            case Meth of
                'GET' ->
                    serve_job_resource(Job, Resource);
                Other ->
                    Htx:method_not_allowed()
            end
    end.

list_jobs(Htx) -> notyet.
create_job(Htx) -> notyet.
job_info(Htx,_) -> notyet.
cancel_job(Htx,_) -> notyet.
serve_job_resource(Htx,_) -> notyet.
