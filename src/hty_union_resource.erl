%% Author: jens
%% Created: 31 dec 2012
%% Description: TODO: Add description to hty_union_resource
-module(hty_union_resource).
-record(hty_union_resource, {subs}).

%%
%% Exported Functions
%%
-export([handle/2, new/1, mount/2]).

%%
%% API Functions
%%
mount(Fspath, Mc) ->
  case hty_fspath:ext(Fspath) of
    "union" ->
      {ok, assemble(Fspath, Mc)}
	end.

new(Subs) -> #hty_union_resource{subs=Subs}.

handle(Htx, This) ->
    hty_tx:dispatch(This#hty_union_resource.subs, Htx).

assemble(Fspath, Mc) ->
  {ok, Subs} = hty_mounter:walk(Fspath, "resource", Mc),
  %A list of {ok, Res, Path, Rule}|{no, Reason...}
  %Sort the OKs after prefix Number segment.
	%create a union resource instance
	Cmp = fun compare/2,
	Subs1 = lists:sort(Cmp, Subs),
	Resources = lists:flatmap(fun({ok, {resource, Resource}, _, _}) ->
									  [Resource];
								 (_) -> []
							  end, Subs1),
	hty_union_resource:new(Resources).

extract_position({_,_,X,_}) ->
    X1 = hty_fspath:new(X),
    [X2|_] = hty_fspath:parts(X1),
    list_to_integer(X2).

compare(A, B) ->
	A1 = extract_position(A),
	B1 = extract_position(B),
	A1 < B1.
