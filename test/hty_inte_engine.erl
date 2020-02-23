-module(hty_inte_engine).

-export([run/2, main/1]).

run(Testdir, Test) ->
  io:format(user, "Running test ~p/~p~n", [Testdir, Test]),
    Specfile = filename:join(Testdir, "testspec.xml"),
    Profile = list_to_atom(Test),
    io:format(user, "About to start httpc with profile ~p~n", [Profile]),
    {ok, HttpcPid} = inets:start(httpc, [{profile, Profile}]),
    true = filelib:is_file(Specfile),
    {Doc, _} = xmerl_scan:file(Specfile),
    Transactions = xmerl_xpath:string("/testspec/transaction", Doc),
    Results = lists:map(fun(Tx) ->
      [Request] = select("request", Tx, Doc),
      Method = case select("method", Request, Doc) of
        [] ->
          get;
        [Elm] ->
          list_to_atom(text(Elm))
      end,

      UrlOrError = case select("url", Request, Doc) of
        [] ->
          {error, url_element_missing};
        [UrlElement] ->
          {ok, text(UrlElement)}
      end,

      Headers = lists:map(fun(HeaderElm) ->
          [Name] = select("@name", HeaderElm, Doc),
          Value = text(HeaderElm),
          {attr(Name), Value}
        end,
        select("header", Request, Doc)
      ),

      EntityOrError = case select("body", Request, Doc) of
        [Body] ->
          case select("@file", Body, Doc) of
            [Attr] ->
              File = attr(Attr),
              file:read_file(File);
            _ ->
              {ok, <<"">>}
          end;
        [] ->
          {ok, <<"">>}
      end,

      case {EntityOrError, UrlOrError} of
        {{ok, Entity},{ok, Url}} ->
          Req = case Entity of
            <<"">> ->
              {Url, Headers};
            NonEmpty ->
              ContentType = lists:keyfind("Content-Type", 1, Headers),
              {Url, Headers, ContentType, NonEmpty}
          end,

          case httpc:request(Method, Req, [], [], Profile) of
            {error, Error} ->
              {error, Error};
            {ok, HttpResponse} ->
              [ResponseAsserts] = select("response", Tx, Doc),
              check_asserts(HttpResponse, ResponseAsserts, Doc, Testdir, Test)
          end;

        {{error, EntityError}, {error, UrlError}} ->
          {error, [EntityError, UrlError]} ;
        {{error, Error}, _} ->
          {error, Error};
        {_, {error, Error}} ->
          {error, Error}
      end
    end, Transactions),
    io:format("Stopping http client"),
    inets:stop(httpc, HttpcPid),
    Results.

check_asserts(Response, Asserts, Doc, Testdir, _TestID) ->
  MakeCheck = fun(Assert) ->
    Target = case tagname(Assert) of
      status ->
        integer_to_list(element(2, element(1, Response)));
      header ->
        Headers = element(2, Response),
        [Attr] = select("@name", Assert, Doc),
        Header = attr(Attr),
        case lists:keyfind(Header, 1, Headers) of
          false ->
            undefined;
          {_Name, Value} ->
            Value
        end;
      body ->
        element(3, Response)
    end,

    Elmer = fun(Elm) ->
      {tagname(Elm), read(Elm, Doc, Testdir), Target, atom_to_list(tagname(Assert))}
    end,

    case select("*|text()", Assert, Doc) of
      [One] ->
        case element(1, One) of
          xmlElement ->
            [Elmer(One)];
          xmlText ->
            [{exact, text(One), Target, atom_to_list(tagname(Assert))}]
        end;
      Several ->
        lists:map(fun(Elm) ->
            case element(1, Elm) of
              xmlElement ->
                Elmer(Elm);
              _ ->
                []
            end
        end, Several)
    end
  end,

  Checks = lists:flatten(lists:map(MakeCheck, select("*", Asserts, doc))),
  lists:foreach(fun(Check) ->
    case Check of
      {exact, A, A, _AssertName} -> ok;
      {exact, Expected, Actual, _AssertName} ->
        throw({error, ["Expected ", Expected, " but got ", Actual]});
      {match, Pattern, Actual, AssertName} ->
        case re:compile(Pattern) of
          {ok, Mp} ->
            case re:run(Actual, Mp) of
              {match, _} -> ok;
              nomatch ->
               throw({error, ["Expected match agains ", Pattern, " but got ", Actual]})
            end;
          {error, {Error, Pos}} ->
            throw({error, ["Bad regex", AssertName, Error, Pos]})
        end
    end
  end, Checks).

  read(Elm, Doc, Dir) ->
      case select("@file", Elm, Doc) of
  	[] ->
  	    text(Elm);
  	[Attr] ->
  	    case file:read_file(filename:join(Dir, attr(Attr))) of
  		{ok, Binary} ->
  		    binary_to_list(Binary);
  		{error, Error} ->
  		    {error, Error}
  	    end
      end.

select(Xpath, Node, Doc) ->
  xmerl_xpath:string(Xpath, Node, [], Doc, []).
text({xmlText, _, _, _, Text, _}) ->
  Text;
text({xmlElement,_,_,_,_,_,_,_,[XmlText], _, _, _}) ->
  text(XmlText).
attr({xmlAttribute, _, _, _, _, _, _, _, Value, _}) ->
  Value.
tagname({xmlElement,TagName     ,_,_,_,_,_,_,_,_, _, _}) ->
  TagName.

  main(_) ->
    Basedir = "src/test/inte/",
    inets:start(),
    Tests = case file:list_dir(Basedir) of
      {ok, Fixtures} ->
        lists:map(fun(Fixture) ->
          Etcdir = filename:join(["src", "test", "inte", Fixture, "fixture"]),
          Setup = fun() ->
            {ok, _} = hty_main:start([Etcdir])
          end,
          Teardown = fun(_) ->
            ok = hty_main:stop()
          end,
          {foreach, Setup, Teardown, tests_in_fixture(Basedir, Fixture)}
        end, Fixtures);
      {error, Error} ->
        io:format("Failed running fixtures in ~p, got error ~p~n", [Basedir, Error]),
        {"Create fixtures", fun() -> throw({error, Error}) end}
    end,
    Tests.

  tests_in_fixture(Basedir, Fixture) ->
    Fixturedir = filename:join(Basedir, Fixture),
    TestFolder = filename:join(Fixturedir, "tests"),
    case file:list_dir(TestFolder) of
      {ok, Tests} ->
        lists:map(fun(Test) ->
          {Fixture ++ ":" ++ Test, fun() -> hty_inte:run(filename:join(TestFolder, Test), Test) end}
        end, Tests);
      {error, enoent} ->
        io:format("Found no tests in ~p~n", [TestFolder]),
        [];
      {error, Error} ->
        Msg = io_lib:format("Fixture ~p got error ~p ~n",
          [Fixture, Error]),
        [fun() -> fail = Msg end]
    end.
