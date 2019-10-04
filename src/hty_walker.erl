%@doc This is a take on a walker that works not on the file system but on http
%resources. It is meant to replace the mechanism
%     using fs_cursor as the rule engine for mounting resource classes.
%     This would mean that a resource instance would not have access to an Fspath
% but other resources, some of which may be special fs

%resource that DOES have fs access. The important consideration is sendfile.
%But some module would have to populate HTX with
% a file output. A resource that wants to stream a file to client
%would then synt a Htx2 and send it to the underlying resource.
% then a function like hty_tx:pipe(htx()) would ask the actual
%tx to copy output directives from the synted one.
% In many cases the real tx could probably just be reused also.
-module(hty_walker).

-record(hty_walker, {fspath, rules=[], transforms=[]}).

-export([new/3, fspath/1,fspath/2, rules/1, rules/2, transforms/1, transforms/2]).
-export([match/1, walk/1, walk/2, subs/1, subs/2]).

new(Fspath, Rules, Transforms) ->
  #hty_walker{fspath=Fspath, rules=Rules, transforms=Transforms}.

fspath(This) ->
  This#hty_walker.fspath.

fspath(Fspath, This) ->
  This#hty_walker{fspath=Fspath}.

rules(This) ->
  This#hty_walker.rules.

rules(Rules, This) ->
  This#hty_walker{rules=Rules}.

transforms(This) ->
    This#hty_walker.transforms.

transforms(Transforms, This) ->
    This#hty_walker{transforms=Transforms}.

match(This) ->
  Rules = This:rules(),
  rec_match(Rules, This).

rec_match([], This) ->
  {no, orphan, path(This), This:rules()};
rec_match([Rule|Rules], This) ->
      Path = path(This),
      case Rule:match(This) of
    {claim, Response} ->
        {ok, Response, Path, Rule};
    block ->
        {no, blocked, Path, Rule};
    {block, Why} ->
        {no, {blocked, Why}, Path, Rule};
    next ->
        rec_match(Rules, This)
      end.

path(This) ->
  (This#hty_walker.fspath):path().

walk(This) ->
    walk(none, This).

walk(Filter, This) ->
    Fspath = This:fspath(),
    List = case Filter of
           none -> Fspath:list();
           Filter1 -> Fspath:list(Filter1)
    end,
    lists:map(fun(Fspath2) ->
            (This:fspath(Fspath2)):match()
          end, List).

subs(This) ->
      subs(none, This).
subs(Filter, This) ->
      lists:flatmap(fun({ok, {resource, R}, _, _}) -> [R];
           (Other) ->
          io:format("subs:~p~n", [Other]), []
        end, walk(Filter, This)).