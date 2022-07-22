-module(hty_inte_SUITE).

-export([accesslog_check_request_logged/1]).
-export([catch_forward/1]).
-export([history_empty/1,
      history_unparsable_body/1,
      history_post_and_reread/1,
      history_get_latest/1,
      history_get_earlier/1,
      history_expect_conflict/1]).
-export([move_file/1,
        move_folder/1]).
-export([static_get_rootfile/1,
        static_get_subfile/1,
        static_index_txt_welcome/1,
        static_get_rootindex/1,
        static_get_subindex/1]).
-export([staticlisting_list_root/1,
        staticlisting_list_sub/1,
        staticlisting_prefer_welcomefile/1]).
-export([vhosts_basic/1,
      vhosts_subdomains/1]).
-export([xslpi_basic/1]).

-include("hty_test.hrl").

all() -> [
    {group, accesslog},
    {group, 'catch'},
    {group, move},
    {group, history},
    {group, static},
    {group, staticlisting},
    {group, vhosts},
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
        move_file,
        move_folder
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
    {vhosts, [
      vhosts_basic,
      vhosts_subdomains
    ]},
    {xslpi, [
      xslpi_basic
    ]}
].



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
        url = "http://localhost:1034/data/files/oldname",
        rsp_pattern = <<"Hello, world!">>
    },
    #exchange{
        url = "http://localhost:1034/data/files/newname",
        status = 404
    },
    #exchange{
        url = "http://localhost:1034/data/files/oldname",
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
        url = "http://localhost:1034/data/files/newname",
        rsp_pattern = <<"Hello, world!">>
    },
    #exchange{
        url = "http://localhost:1034/data/files/oldname",
        status = 404
    }], Cfg).
    
move_folder(Cfg) -> run_test([
    #exchange{
        url = "http://localhost:1034/data/files/oldfolder/filename",
        rsp_pattern = <<"Hello, world!">>
    },
    #exchange{
        url = "http://localhost:1034/data/files/newfolder/filename",
        status = 404
    },
    #exchange{
        url = "http://localhost:1034/data/files/oldfolder/filename",
        method = delete,
        req_headers = [
            {"destination", "/data/files/newfolder/filename"},
            {"overwrite", "F"},
            {"x-http-method-override", "MOVE"}
        ],
        rsp_headers = [
            {"location", "/data/files/newfolder/filename"}
        ]
    },
    #exchange{
        url = "http://localhost:1034/data/files/newfolder/filename",
        rsp_pattern = <<"Hello, world!">>
    },
    #exchange{
        url = "http://localhost:1034/data/files/oldfolder/filename",
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

vhosts_basic(Cfg) ->
  run_test([#exchange{
        url = "http://localhost:1035/basic/index.txt",
        req_headers = [
          {"accept", "text/plain"},
          {"host", "orangutang"}
        ],
        status = 200,
        rsp_body = <<"Hello Orangutan!">>
  }, #exchange{
        url = "http://localhost:1035/basic/index.txt",
        req_headers = [
          {"accept", "text/plain"},
          {"host", "schimpans"}
        ],
        status = 200,
        rsp_body = <<"Hello Chimpanzee!">>
  }], Cfg).

  vhosts_subdomains(Cfg) ->
    run_test([#exchange{
          url = "http://localhost:1035/subdomains/index.txt",
          req_headers = [
            {"accept", "text/plain"},
            {"host", "orangutang.jenspersson.com"}
          ],
          status = 200,
          rsp_body = <<"Hello Orangutan!">>
    }, #exchange{
          url = "http://localhost:1035/subdomains/index.txt",
          req_headers = [
            {"accept", "text/plain"},
            {"host", "schimpans.jenspersson.com"}
          ],
          status = 200,
          rsp_body = <<"Hello Chimpanzee!">>
    }], Cfg).
  