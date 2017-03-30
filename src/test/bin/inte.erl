#!/usr/bin/env escript

basedir() -> "src/test/inte/".

select(Xpath, Node, Doc) ->
  xmerl_xpath:string(Xpath, Node, [], Doc, []).
text({xmlText, _, _, _, Text, _}) ->
  Text;
text({xmlElement,_,_,_,_,_,_,_,[XmlText], _, _, _}) ->
  text(XmlText).
attr({xmlAttribute, _, _, _, _, _, _, _, Value, _}) ->
  Value.
tagname({xmlElement,TagName,_,_,_,_,_,_,_,_, _, _}) ->
  TagName.

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

check_asserts(Response, Asserts, Doc, Testdir, TestID) ->
    MakeCheck =
	fun(Assert) ->
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
    lists:map(fun(Check) ->
		      case Check of
			  {exact, A, A, AssertName} ->
			      {pass, TestID, AssertName};
			  {exact, A, B, AssertName} ->
			      {fail, TestID, AssertName, A, B};
			  Unknown ->
			      {fail, TestID, unknown, Unknown}
		      end
	      end, Checks).


run_test(Fixturedir, Test) ->
  Testdir = filename:join([Fixturedir, "tests", Test]),
  io:format("Running test ~p in ~p~n",[Test, Testdir]),
  Specfile = filename:join(Testdir, "testspec.xml"),
  Profile = list_to_atom(Test),
  {ok, HttpcPid} = inets:start(httpc, [{profile, Profile}]),
  {Doc, _} = xmerl_scan:file(Specfile),
  Transactions = xmerl_xpath:string("/testspec/transaction", Doc),
  Results0 = lists:map(fun(Tx) ->
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

    io:format("Assembling request headers...~n"),
    Headers = lists:map(fun(HeaderElm) ->
        Attr = select("@name", HeaderElm, Doc),
        apa = HeaderElm,
        {}
      end,
    select("header", Request, Doc)),

    io:format("Assembling request entity...~n"),
    EntityOrError = case select("body", Request, Doc) of
      [Body] ->
        case select("@file", Body, Doc) of
          [Attr] ->
            File = Testdir ++ [$/] ++ attr(Attr),
            case file:read_file(File) of
              {ok, Fd} -> {ok, Fd};
              {error, Er} -> 
                io:format("Could not read file attr [~p] of body...~n", [File]),
                {error, Er}
            end;
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
            ContentType = case lists:keyfind("Content-Type", 1, Headers) of
              false -> "application/octet-stream";
              ActualContentType -> ActualContentType
            end,
            {Url, Headers, ContentType, NonEmpty}
        end,
        io:format("Sending request~n"),
        Rsp = case httpc:request(Method, Req, [], [], Profile) of
          {error, Error} ->
            {error, Error};
          {ok, HttpResponse} ->
            io:format("Checking asserts ~n"),
            [ResponseAsserts] = select("response", Tx, Doc),
            io:format("Checking2 asserts ~n"),
            check_asserts(HttpResponse, ResponseAsserts, Doc, Testdir, Test);
          Other ->
            io:format("httpc response: ~p~n", [Other])
        end,
        io:format("Rsp ~p~n", [Rsp]),
        Rsp;
      {{error, EntityError}, {error, UrlError}} ->
        {error, [EntityError, UrlError]};
      {{error, Error}, _} ->
        {error, Error};
      {_, {error, Error}} ->
        {error, Error}
    end
  end, Transactions),
  inets:stop(httpc, HttpcPid),
  io:format("Results0 ~p~n", [Results0]),
  %[Results] = Results0, 
  {testresult, Testdir, Results0}.

spawn_fixture(ReplyTo, Etcdir) ->
  io:format("Starting fixture ~p~n", [Etcdir]),
  process_flag(trap_exit, true),
  PortArgs = [
    exit_status, stream, eof, use_stdio, stderr_to_stdout
  ],
  try open_port({spawn, "./hty " ++ Etcdir}, PortArgs) of
    Port -> 
      loop_expect(ReplyTo, Port)
  catch 
    Error -> io:format("Error ~p ~n", [Error]), {error, Error}
  end.
  
loop_expect(ReplyTo, Port) ->
  receive
    {Port, {data, Data}} ->
      io:format("Got data from port ~p~n", [Data]),
%      {ok, Regex} = re:compile("hty start status: ([^ ]+)"),
      Regex = "hty start status: ([^ ]+)",
      case re:run(Data, Regex, [{capture, all_but_first, list}]) of
        {match, [Capture]} ->
          io:format("Capture ~p~n", [Capture]),
          case Capture of
            "started" ->
              io:format("Data matches expected, good to go~n"),
              ReplyTo ! go,
              loop_fixture(Port);
            "fail" ->
              io:format("Fixture Failure"),
              ReplyTo ! no
          end;
        nomatch ->
          io:format("Data doesnt match expected, wait~n"),
          loop_expect(ReplyTo, Port)
      end
  after 60000 ->
    {error, fixture_expect_timeout}
  end.


loop_fixture(Port) ->
  receive 
    stop -> 
      io:format("Fixture stopping ~p~n", [Port]),
      port_close(Port),
      receive 
        {Port, {exit_status, Status}} ->
          io:format("Fixture stopped with status ~p~n", [Status]);
        {'EXIT', Port, Reason} ->
          io:format("Fixture2 trapped exit ~p~n", [Reason])
      end;
    {_, {data, Data}} ->
      io:format("Got data from port ~p~n", [Data]),
      loop_fixture(Port);
    {'EXIT', Port, Reason} ->
      io:format("Fixture trapped exit ~p~n", [Reason])
  end.

run_fixture(Fixture) ->
  io:format("About to start fixture ~p~n", [Fixture]),
  Fixturedir = basedir() ++ Fixture,
  Etcdir = filename:join(["src", "test", "inte", Fixture, "fixture"]),
  
  ReplyTo = self(),
  FixtureActor = spawn(fun() -> spawn_fixture(ReplyTo, Etcdir) end),
  receive 
    go -> 
      TestFolder = filename:join(Fixturedir, "tests"),
      Retval = case file:list_dir(TestFolder) of
        {ok, Tests} ->
          io:format("Found these tests ~p~n", [Tests]),
    	    TestResults = lists:map(fun(Test) ->
    	      run_test(Fixturedir, Test)
    	    end, Tests),
          {Fixture, TestResults};
        {error, enoent} ->
          io:format("Found no tests in ~p~n", [TestFolder]),
          {Fixture, []}
      end,
      FixtureActor ! stop, 
      Retval;
    no ->
      io:format("Fixture failed~n"),
      {Fixture, []}
  end.

%command(Command) ->
%  Port = open_port([{spawn, Command}]),

main([Outfile]) ->
    Basedir = basedir(),
    inets:start(),
    %code:add_pathz("target/ebin"),
    FixtureResults = case file:list_dir(Basedir) of
	     {ok, Fixtures} ->
	        io:format("Found these fixtures: ~p~n", [Fixtures]),
	        lists:map(
	          fun(Fixture) ->
              FixtureResult = run_fixture(Fixture),
				      io:format("Fixture ~p ~p~n", [Fixture, FixtureResult]),
              FixtureResult 
            end, Fixtures);
	     {error, Error} ->
	        io:format("Failed running fixtures in ~p, got error ~p~n", [Basedir, Error]),
          {error, Error}
    end,
    {ok, Out} = file:open(Outfile, [write]),

    WriteAssert = fun(Out, TestResult) ->
      case TestResult of
        {pass, Test, Assert} ->
          file:write(Out, [<<"<pass assert=\"">>, 
                          [Test ++ "/" ++ Assert], <<"\"/>">>, 10]);
        {fail, Test, Assert, Expected, Actual} ->
          file:write(Out, [<<"<fail assert=\"">>, [Test ++ "/" ++ Assert], <<"\">">>]),
          file:write(Out, [<<"<expected><![CDATA[">>,Expected,<<"]]></expected>">>]),
          file:write(Out, [<<"<actual><![CDATA[">>,Actual,<<"]]></actual>">>]),
          file:write(Out, [<<"</fail>">>, 10])
      end
    end,

    WriteTestResult = fun(A) ->
      io:format("A=~p~n", [A]),
      {testresult, TestName, TestResults} = A,
      file:write(Out, [<<"<test path=\"">>, TestName, <<"\">">>, 10]),
      foreach(TestResults, 
        fun(TestResult) ->
          io:format("TestResult ~p~n", [TestResult]),
          case is_list(TestResult) of
            true -> 
              foreach(TestResult, fun(Assert) -> WriteAssert(Out, Assert) end);
            false ->
              WriteAssert(Out, TestResult)
          end
        end),
      file:write(Out, [<<"</test>">>, 10])
    end,

    WriteFixture = fun(A) ->
      {Fixture, TestResults} = A,
      file:write(Out, [<<"<fixture path=\"">>, Fixture, <<"\">">>, 10]),
      lists:foreach(WriteTestResult, TestResults),
      file:write(Out, [<<"</fixture>">>,10])
    end,

    file:write(Out, [<<"<fixtures>">>, 10]),
    lists:foreach(WriteFixture, FixtureResults),
    file:write(Out, <<"</fixtures>">>),
    file:close(Out),
    inets:stop().


foreach(List, Fun) -> lists:foreach(Fun, List).
