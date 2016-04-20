%% Author: jens
%% Created: 28 jan 2013
%% Description: TODO: Add description to hty_xslpi_resource
-module(hty_xslpi_resource).
-record(hty_xslpi_resource,  {subs}).

-export([handle/2, new/1]).
-export([mount/1]).

mount(Fspath) ->
  {ok, Subs} = hty_mounter:walk(Fspath, "resource"),
  {ok, new(Subs)}.

new(Subs) ->
    #hty_xslpi_resource{subs=Subs}.

handle(Htx, This) ->
    %XslpiRules = This#hty_xslpi_resource.xslpirules,
    Subs = This#hty_xslpi_resource.subs,
    HtxSub = hty_tx_factory:get(<<"xslpi">>),
    HtxSub1 = HtxSub:dispatch(Subs),
    Htx1 = Htx:echo(<<"<?xml-stylesheet type=\"text/xsl\" href=\"">>),
    Htx2 = Htx1:copy(HtxSub1),
    Htx2:echo(<<".xsl\"?>">>).
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
%xslpi(XslURL, Htx2) ->
%    [
%     hty_percentencoding:decode(list_to_binary(XslURL)),
%     .
