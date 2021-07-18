-module(hty_exec_resource).
-record(hty_exec_resource, {subcommander}).
-export([mount/2, handle/2]).

mount(_Fspath, _Mc) ->
  % We need two params: subcommander name and workdir
  {ok, #hty_exec_resource{subcommander=subcommander:start()}}.

handle(Htx, _This) ->
    Meth = Htx:method(),
    case Htx:path_below() of
        [] ->
            case Meth of
                'GET' ->
                    list_jobs(Htx);
                'POST' ->
                    create_job(Htx);
                _Other ->
                    Htx:method_not_allowed()
            end;
        [Job] ->
            case Meth of
                'GET' ->
                    job_info(Job, Htx);
                'DELETE' ->
                    cancel_job(Job, Htx);
                _Other ->
                    Htx:method_not_allowed()
            end;
        [Job, Resource] ->
            case Meth of
                'GET' ->
                    serve_job_resource(Job, Resource);
                _Other ->
                    Htx:method_not_allowed()
            end
    end.

list_jobs(_Htx) -> notyet.
create_job(_Htx) -> notyet.
job_info(_Htx,_) -> notyet.
cancel_job(_Htx,_) -> notyet.
serve_job_resource(_Htx,_) -> notyet.
