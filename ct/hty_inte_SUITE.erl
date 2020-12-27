-module(hty_inte_SUITE).
-compile(export_all).

all() -> [
    hello,
    {group, accesslog},
    {group, 'catch'},
    {group, move},
    {group, history},
    {group, static},
    {group, staticlisting},
    {group, xslpi}
].

groups() -> [
    {accesslog, [
      accesslog_check_request_logged
    ]},
    {'catch', [
        catch_forward
    ]},
    {history, [
      history_empty,
      history_unparsable_body,
      history_post_and_reread,
      history_get_latest,
      history_get_earlier,
      history_expect_conflict
    ]},
    {move, [
        move_file
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
    ]},
    {xslpi, [
      xslpi_basic
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
            Reqbody
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
                strict -> OtherBody = Body;
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

xslpi_basic(Config) ->
  run_test(#exchange{
    url = "http://localhost:1030/docs/resource.xml",
    rsp_headers = [
      {"content-type", "text/xml"}
    ],
    rsp_file = "facit.xml"
    },
    Config).

accesslog_check_request_logged(Cfg) -> run_test([
  #exchange{
    url = "http://localhost:1028/helloworld.txt",
    rsp_headers = [
      {"content-type", "text/plain"}
    ],
    rsp_file = "facit.txt"
  },
  #exchange{
    url = "http://localhost:1028/logs",
    rsp_headers = [
      {"content-type", "text/plain"}
    ],
    rsp_pattern = "^.*|GET /helloworld.txt| .* 200"
    }
  ], Cfg).
  
  
move_file(Cfg) -> run_test([
    #exchange{
        url = "http://localhost:1034/oldname",
        rsp_pattern = <<"Hello, world!">>
    },
    #exchange{
        url = "http://localhost:1034/newname",
        status = 404
    },
    #exchange{
        url = "http://localhost:1034/oldname",
        method = delete,
        req_headers = [
            {"destination", "newname"},
            {"overwrite", "F"},
            {"x-http-method-override", "MOVE"}
        ],
        rsp_headers = [
            {"location", "newname"}
        ]
    },
    #exchange{
        url = "http://localhost:1034/newname",
        rsp_pattern = <<"Hello, world!">>
    },
    #exchange{
        url = "http://localhost:1034/oldname",
        status = 404
    }], Cfg).

history_unparsable_body(Cfg) ->
  run_test(
    #exchange{
      method = post,
      url = "http://localhost:1033/unparsable.xml",
      status = 400,
      req_headers = [
        {"content-type", "application/x-www-form-urlencoded"}
      ],
      req_body = <<"[=root&\"=lite_text&]=annan_root">>
    }
  ,Cfg).

history_get_latest(Cfg) -> run_test([
  #exchange{
    url = "http://localhost:1033/get_latest.xml",
    method = post,
    req_headers = [
      {"content-type", "application/x-www-form-urlencoded"}
    ],
    req_body = "[=apor&[=apa&\"=orangutang&]=apa&]=apor"
  },
  #exchange{
    url = "http://localhost:1033/get_latest.xml",
    method = post,
    req_headers = [
      {"content-type", "application/x-www-form-urlencoded"}
    ],
    req_body = "[=apor&[=apa&\"=bonobo&]=apa&]=apor"
  },
  #exchange{
    url = "http://localhost:1033/get_latest.xml",
    rsp_body = "<apor><apa>bonobo</apa></apor>",
    entity_compare = trimmed
  }
  ], Cfg).


history_get_earlier(Cfg) -> run_test([
  #exchange{
    url = "http://localhost:1033/get_earlier.xml",
    method = post,
    req_headers = [
      {"content-type", "application/x-www-form-urlencoded"}
    ],
    req_body = "[=apor&[=apa&\"=orangutang&]=apa&]=apor"
  },
  #exchange{
    url = "http://localhost:1033/get_earlier.xml",
    method = post,
    req_headers = [
      {"content-type", "application/x-www-form-urlencoded"}
    ],
    req_body = "[=apor&[=apa&\"=bonobo&]=apa&]=apor"
  },
  #exchange{
    url = "http://localhost:1033/get_earlier.xml;rev=0",
    rsp_body = "<apor><apa>orangutang</apa></apor>",
    entity_compare = trimmed
  }
], Cfg).

history_expect_conflict(Cfg) -> run_test([
  #exchange{
    url = "http://localhost:1033/expect_conflict.xml",
    method = post,
    req_headers = [
      {"content-type", "application/x-www-form-urlencoded"}
    ],
    req_body = "[=apor&[=apa&\"=orangutang&]=apa&]=apor"
  },
  #exchange{
    url = "http://localhost:1033/expect_conflict.xml;rev=0",
    method = post,
    req_headers = [
      {"content-type", "application/x-www-form-urlencoded"}
    ],
    req_body = "[=apor&[=apa&\"=bonobo&]=apa&]=apor"
  },
  #exchange{
    url = "http://localhost:1033/expect_conflict.xml;rev=0",
    method = post,
    req_headers = [
      {"content-type", "application/x-www-form-urlencoded"}
    ],
    status = 409,
    req_body = "[=apor&[=apa&\"=gorilla&]=apa&]=apor"
  },
  #exchange{
    url = "http://localhost:1033/expect_conflict.xml",
    rsp_body = "<apor><apa>bonobo</apa></apor>",
    entity_compare = trimmed
  }
], Cfg).

history_post_and_reread(Cfg) ->
  run_test([
    #exchange{
      url = "http://localhost:1033/apor.xml",
      method = post,
      req_headers = [
        {"content-type", "application/x-www-form-urlencoded"}
      ],
      req_body = "[=apor&[=apa&\"=orangutang&]=apa&]=apor"
    },
    #exchange{
      url = "http://localhost:1033/apor.xml",
      rsp_file = "facit.xml",
      entity_compare = trimmed
    }
  ], Cfg).

history_empty(Config) ->
  run_test(#exchange{
    url = "http://localhost:1033/finns_inte.xml",
    status = 404
  }, Config).
