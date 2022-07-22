-export([
  all/0, groups/0, 
  init_per_group/2, end_per_group/2, 
  init_per_suite/1, end_per_suite/1,
  init_per_testcase/2, end_per_testcase/2]).

init_per_suite(Config) ->
    ok = inets:start(),
    Config.

end_per_suite(Config) ->
    ok = inets:stop(),
    Config.

init_per_group(Group, Config) ->
    Groupname = atom_to_list(Group),
    Groupdir = proplists:get_value(data_dir, Config) ++ Groupname,
    Fromdir = Groupdir ++ "/fixture",
    Todir = proplists:get_value(priv_dir, Config) ++ Groupname ++ "/fixture",
    copy_fixture(Fromdir, Todir),
    {ok, _Pid} = hty_main:start([Todir]),
    [{wwwroot, Todir},
     {files_dir, Groupdir ++ "/files"}|Config].

end_per_group(_Group, Config) ->
    ok = hty_main:stop(),
    Config.

init_per_testcase(Test, Config) ->
    io:format(user, "About to start httpc with profile ~p~n", [Test]),
    {ok, _} = inets:start(httpc, [{profile, Test}]),
    [{tc_name, Test}|Config].

end_per_testcase(Test, _Config) ->
    %Httpcpid = proplists:get_value(httpc_pid, Config, no_httpc_pid_config),
    ok = inets:stop(httpc, Test).

%%% Support
copy_fixture(From, To) ->
    case filelib:is_dir(From) of
        true ->
            filelib:ensure_dir(To),
            file:make_dir(To),
            {ok, Filenames} = file:list_dir(From),
            lists:foreach(fun(Filename) ->
                copy_fixture(From ++ "/" ++ Filename, To ++ "/" ++ Filename)
            end, Filenames);
        false ->
            ok = filelib:ensure_dir(To),
            io:format("Copying from ~s to ~s", [From, To]),
            {ok, _} = file:copy(From, To)
    end.

-record(exchange, {
    method = get,
    req_headers = [],
    req_file = none,
    req_body = none,
    url = no_url,
    status = 200,
    rsp_headers = [],
    rsp_body = no_rsp_body,
    rsp_file = no_rsp_file,
    rsp_pattern = no_rsp_pattern,
    entity_compare = strict :: strict | trimmed
}).

run_test(Exchanges, Config) ->
    Testcase = proplists:get_value(tc_name, Config),
    run_test(Testcase, Exchanges, Config).

run_test(Test, Exchange, Config) when is_record(Exchange, exchange) ->
  run_test(Test, [Exchange], Config);
run_test(Test, Exchanges, Config) when is_list(Exchanges) ->
    Filesdir = proplists:get_value(files_dir, Config)
      ++ "/" ++ atom_to_list(Test) ++ "/",
    lists:foreach(fun(X) ->
        Request = case {X#exchange.req_file, X#exchange.req_body} of
          {none, none} -> {
              X#exchange.url,
              X#exchange.req_headers
          };
          {Reqfile, none} -> {
              X#exchange.url,
              X#exchange.req_headers,
              proplists:get_value("content-type",
                X#exchange.req_headers, "application/octet-stream"),
              hty_result:assume_ok(file:read_file(Filesdir ++ Reqfile))
          };
          {none, Reqbody} -> {
            X#exchange.url,
            X#exchange.req_headers,
            proplists:get_value("content-type",
              X#exchange.req_headers, "application/octet-stream"),
            case is_binary(Reqbody) of
              true -> Reqbody;
              false -> list_to_binary(Reqbody)
            end
          };
          {_,_} -> error(dont_specify_both_req_file_and_req_body)
        end,
        {ok, Response} = httpc:request(
            X#exchange.method,
            Request,
            [], % HttpOptions
            [{body_format, binary}], % Options
            Test),
        {{_, Status, StatusMessage}, ResponseHeaders, Body} = Response,
        case X#exchange.status of
            undefined -> ok;
            Status -> ok;
            _ -> notok = io:format("Wrong status [~p ~p]~n", [Status, StatusMessage])
        end,
        lists:foreach(fun({Name, Value}) ->
            io:format("Looking for header ~s=~s in ~p~n", [Name, Value, ResponseHeaders]),
            true = lists:member({Name, Value}, ResponseHeaders)
        end, X#exchange.rsp_headers),

        case X#exchange.rsp_pattern of
          no_rsp_pattern -> ok;
          Pattern ->
            case re:compile(Pattern) of
              {ok, Mp} ->
                case re:run(Body, Mp) of
                  {match, _} -> ok;
                  nomatch ->
                   throw({error, ["Expected match against ", Pattern, " but got ", Body]})
                end;
              {error, {Error, Pos}} ->
                throw({error, ["Bad regex", Error, Pos]})
            end
        end,

        case X#exchange.rsp_body of
            no_rsp_body -> ok;
            Body -> ok;
            OtherBody ->
              case X#exchange.entity_compare of
                strict -> 
                  throw({error, ["Expected ", Body, " but got ", OtherBody]});  
                trimmed -> trim_compare(OtherBody, Body)
              end
        end,
        case X#exchange.rsp_file of
            no_rsp_file -> ok;
            Filename ->
                Filepath = Filesdir ++ Filename,
                io:format("Reading facit file ~s~n", [Filepath]),
                {ok, Facit} = file:read_file(Filepath),
                io:format("About to compare facit [~s]~n with response body [~s]~n", [Facit, Body]),
                case X#exchange.entity_compare of
                  strict -> Facit = Body;
                  trimmed -> trim_compare(Facit, Body)
                end
        end
    end, Exchanges).

trim_compare(Bin1, Bin2) when is_binary(Bin1) ->
  trim_compare(binary_to_list(Bin1), Bin2);
trim_compare(Str1, Bin2) when is_binary(Bin2) ->
  trim_compare(Str1, binary_to_list(Bin2));
trim_compare(Str1, Str2) ->
  S1 = string:trim(Str1),
  S2 = string:trim(Str2),
  S1 = S2.