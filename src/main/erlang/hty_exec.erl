-module(hty_exec).

-export([run/1]).

%-record(hty_exec_job, {command::binary(), job_id=0}).
%-record(hty_exec_state, {last_id=0}).

-include_lib("kernel/include/file.hrl").

%echo(Msg) ->
%  erlang:display("Hello world " ++ Msg).

run(_Job) ->
  Id = "23",
  Path = "./proc/" ++ Id,
  file:make_dir(Path),
  Command = <<"echo HelloWorld">>,
  file:write_file(Path ++ "/command.sh", Command),
  file:write_file_info(Path ++ "/command.sh", #file_info{mode=8#00700}),
  open_port({spawn, "./wrapper.sh " ++ Path}, []).

%start() ->
%  case spawn(fun() -> loop(#hty_exec_state{}) end) of
%    {ok, Pid} ->
%      {ok, Pid};
%    {error, Error} ->
%      {error, Error}
%  end.

%prepare(X) -> no.

%loop(Cfg) ->
%  receive
%    {exec, Exec} ->
%      LastId = Cfg#hty_exec_state.last_id,
%      JobId = LastId + 1,
%      case prepare(Exec#hty_exec_job{job_id=JobId}) of
%        {ok, Exec1} ->
%          loop(Cfg);
%        _ ->
%          loop(Cfg)
%      end
%  end.
