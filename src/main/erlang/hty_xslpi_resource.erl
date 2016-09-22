%% Author: jens
%% Created: 28 jan 2013
%% Description: TODO: Add description to hty_xslpi_resource
-module(hty_xslpi_resource).
-record(hty_xslpi_resource,  {subs, xslpi}).

-export([handle/2]).
-export([mount/1]).

mount(Fspath) ->
  case lists:reverse(Fspath:parts()) of
    ["xslpi", Url|_] ->
      case hty_mounter:walk(Fspath, "resource") of
        {ok, Subs} ->
          Xslpi = xslpi(Url),
          {ok, #hty_xslpi_resource{subs=Subs, xslpi=Xslpi}};
        {error, Error} ->
          {error, {?MODULE, Error}}
      end;
    Other ->
      {error, "hty_xslpi_resource requires a url param"}
  end.

handle(Htx, This) ->
    %XslpiRules = This#hty_xslpi_resource.xslpirules,
    Subs = This#hty_xslpi_resource.subs,
    Htx1 = Htx:dispatch(Subs),
    case Htx1:status() of
      {200, _} ->
        Htx1:prolog(This#hty_xslpi_resource.xslpi);
      _ ->
        Htx1
    end.
    %Xchoice = case Htx1:bound("xslpi_choice") of
    %  {ok, Xslpichoice} -> Xslpichoice;
    %  no -> "any"
    %end,
    %case lists:keyfind(Xchoice, 1, XslpiRules) of
  %    false ->
  %      Htx1;
  %    {_, Url} ->
  %      Htx1:prolog(xslpi(Url))
  %  end.

%%
%% Local Functions
%%
xslpi(XslURL) ->
  [<<"<?xml-stylesheet type=\"text/xsl\" href=\"">>,
   hty_percentencoding:decode(list_to_binary(XslURL)),
   <<".xsl\"?>">>].
