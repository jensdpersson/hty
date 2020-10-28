-module(hty_inte_SUITE).

-export([
    all/0,
    groups/0,

    init_per_suite/1,
    end_per_suite/1,

    init_per_group/2,
    end_per_group/2,

    init_per_testcase/2,
    end_per_testcase/2,

    hello/1,

    catch_forward/1,

    static_get_rootfile/1,
    static_get_subfile/1,
    static_index_txt_welcome/1,
    static_get_rootindex/1,
    static_get_subindex/1,

    staticlisting_list_root/1,
    staticlisting_list_sub/1,
    staticlisting_prefer_welcomefile/1
]).

%%% Common test

all() -> [
    hello,
    {group, 'catch'},
    {group, static},
    {group, staticlisting}
].

groups() -> [
    {'catch', [
        catch_forward
    ]},
    {static, [
        static_get_rootfile,
        static_get_subfile,
        static_index_txt_welcome,
        static_get_rootindex,
        static_get_subindex
    ]},
    {staticlisting, [
        staticlisting_list_root,
        staticlisting_list_sub,
        staticlisting_prefer_welcomefile
    ]}
].

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

end_per_group(Group, Config) ->
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
    url = no_url,
    status = 200,
    rsp_headers = [],
    rsp_body = no_rsp_body,
    rsp_file = no_rsp_file,
    entity_compare = strict :: strict | trimmed
}).

run_test(Exchanges, Config) ->
    Testcase = proplists:get_value(tc_name, Config),
    run_test(Testcase, Exchanges, Config).

run_test(Test, Exchange, Config) when is_record(Exchange, exchange) ->
  run_test(Test, [Exchange], Config);
run_test(Test, Exchanges, Config) when is_list(Exchanges) ->
    lists:foreach(fun(X) ->
        {ok, Response} = httpc:request(
            X#exchange.method,
            {
                X#exchange.url,
                X#exchange.req_headers
            },
            [], % HttpOptions
            [{body_format, binary}], % Options
            Test),
        {{_, Status, _}, ResponseHeaders, Body} = Response,
        case X#exchange.status of
            undefined -> ok;
            Status -> ok;
            OtherStatus -> OtherStatus = Status
        end,
        lists:foreach(fun({Name, Value}) ->
            io:format("Looking for header ~s=~s in ~p~n", [Name, Value, ResponseHeaders]),
            true = lists:member({Name, Value}, ResponseHeaders)
        end, X#exchange.rsp_headers),

        case X#exchange.rsp_body of
            no_rsp_body -> ok;
            Body -> ok;
            OtherBody ->
              case X#exchange.entity_compare of
                strict -> OtherBody = Body;
                trimmed -> trim_compare(OtherBody, Body)
              end
        end,
        case X#exchange.rsp_file of
            no_rsp_file -> ok;
            Filename ->
                Filepath = proplists:get_value(files_dir, Config)
                    ++ "/" ++ atom_to_list(Test) ++ "/" ++ Filename,
                io:format("Reading facit file ~s~n", [Filepath]),
                {ok, Facit} = file:read_file(Filepath),
                io:format("About to compare facit [~s]~n with response body [~s]~n", [Facit, Body]),
                case X#exchange.entity_compare of
                  strict -> Facit = Body;
                  trimmed -> trim_compare(Facit, Body)
                end
        end
    end, Exchanges).

trim_compare(Bin1, Bin2) ->
  S1 = string:trim(binary_to_list(Bin1)),
  S2 = string:trim(binary_to_list(Bin2)),
  S1 = S2.

hello(Config) -> ok.

catch_forward(Config) ->
  run_test(#exchange{
    url = "http://localhost:1029/helloworld.html",
    rsp_headers = [
        {"content-type", "text/html"}
    ],
    rsp_file = "facit.html"
  }, Config).

static_get_rootfile(Config) ->
    run_test(#exchange{
                url = "http://localhost:1031/rootfile.html",
                rsp_headers = [
                    {"content-type", "text/html"}
                ],
                rsp_file = "facit.html"
            },
            Config).

static_get_subfile(Config) ->
    run_test(#exchange{
                url = "http://localhost:1031/subfolder/subfile.html",
                rsp_headers = [
                    {"content-type", "text/html"}
                ],
                rsp_file = "facit.html"
            },
            Config).

static_index_txt_welcome(Config) ->
    run_test(#exchange{
              url = "http://localhost:1031/subfolderwithtextindex/",
              rsp_headers = [
                  {"content-type", "text/plain"}
              ],
              rsp_file = "facit.txt"
            },
            Config).

static_get_rootindex(Config) ->
  run_test(#exchange{
            url = "http://localhost:1031/",
            rsp_headers = [
                {"content-type", "text/html"}
            ],
            rsp_file = "facit.html"
          },
          Config).

static_get_subindex(Config) ->
  run_test(#exchange{
          url = "http://localhost:1031/subfolder",
          rsp_headers = [
              {"content-type", "text/html"}
          ],
          rsp_file = "facit.html"
        },
        Config).

staticlisting_list_root(Config) ->
  run_test(#exchange{
          url = "http://localhost:1032/",
          rsp_headers = [
              {"content-type", "application/xml"}
          ],
          rsp_file = "facit.xml",
          entity_compare = trimmed
        },
        Config).

staticlisting_list_sub(Config) ->
  run_test(#exchange{
          url = "http://localhost:1032/subfolder/",
          rsp_headers = [
              {"content-type", "application/xml"}
          ],
          rsp_file = "facit.xml",
          entity_compare = trimmed
        },
        Config).

staticlisting_prefer_welcomefile(Config) ->
  run_test(#exchange{
          url = "http://localhost:1032/subfolder/index.html",
          rsp_headers = [
              {"content-type", "text/html"}
          ],
          rsp_file = "facit.html"
        },
        Config).
