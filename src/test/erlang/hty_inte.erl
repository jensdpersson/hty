-module(hty_inte).

-export([run/2]).

run(Testdir, Test) ->
    Specfile = filename:join(Testdir, "testspec.xml"),
    Profile = list_to_atom(Test),
    {ok, HttpcPid} = inets:start(httpc, [{profile, Profile}]),
    true = filelib:is_file(Specfile),
    {Doc, _} = xmerl_scan:file(Specfile),
    Transactions = xmerl_xpath:string("/testspec/transaction", Doc),
    [Results] = lists:map(fun(Tx) ->

      [Request] = select("request", Tx, Doc),

      Method = case select("method", Request, Doc) of
        [] ->
          get;
        Elm ->
         text(Elm)
      end,

      UrlOrError = case select("url", Request, Doc) of
        [] ->
          {error, url_element_missing};
        [UrlElement] ->
          {ok, text(UrlElement)}
      end,

      Headers = lists:map(fun(HeaderElm) ->
          Attr = select("@name", HeaderElm, Doc),
          notsupportedyet = HeaderElm,
          {}
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
    inets:stop(httpc, HttpcPid).

check_asserts(Response, Asserts, Doc, Testdir, TestID) ->
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
  lists:foreach(fun({exact, A, A, AssertName}) -> ok end, Checks).

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
tagname({xmlElement,TagName,_,_,_,_,_,_,_,_, _, _}) ->
  TagName.
