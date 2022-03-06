-module(hty_mounter).
-record(hty_mounter, {prefixes=["hty"], ignore_notfound=false, ignores=[]}).
-export([walk/3, new/1, new/0]).

%-type candidate_result() :: 
%  {ok, Mounted::tuple()} | 
%  {notfound, Candidate::string()} |
%  {error, Error::iolist()}.

new() -> #hty_mounter{}.
new(Opts) when is_list(Opts) -> 
  lists:foldl(fun
    ({prefixes, Prefixes}, O) -> O#hty_mounter{prefixes=Prefixes};
    ({ignore_notfound, IgnoreNotFound}, O) -> O#hty_mounter{ignore_notfound=IgnoreNotFound};
    ({ignores, Ignores}, O) -> O#hty_mounter{ignores=Ignores}
  end, #hty_mounter{}, Opts).
  
mount_children(Fspath, Type, Mounter) ->
  Files0 = sort(hty_fspath:list(Fspath)),
  Files = lists:filter(fun(F) -> 
    case lists:member(F, Mounter#hty_mounter.ignores) of 
      true -> false;
      false -> true
    end 
  end, Files0),
  Inverse = lists:foldl(fun(File, Results) ->
    Result = mount_file(File, Type, Mounter), 
    case Results of 
      {ok, Mounts, Notfounds} ->          
        case Result of
          {ok, Mount} ->
            {ok, [Mount|Mounts], Notfounds};
          {notfound, Notfound} ->
            {ok, Mounts, [Notfound|Notfounds]};
          {error, Error} ->
            {error, Error}
        end;
      {error, Errors} ->
        case Result of
          {ok, _Mount} ->
            {error, Errors};
          {notfound, _Notfound} ->
            {error, Errors};
          {error, Error} ->
            {error, [Error|Errors]}
        end
      end
    end, {ok, [], []}, Files),
  case Inverse of
    {ok, M, N} -> {ok, lists:reverse(M), lists:reverse(N)};
    {error, _} = Error -> Error 
  end.

mount_children_or_fail(Fspath, Type, Mounter) ->
  case mount_children(Fspath, Type, Mounter) of 
    {ok, Mounted, []} -> {ok, Mounted};
    {ok, Mounted, Notmounted} -> 
      case Mounter#hty_mounter.ignore_notfound of 
        true -> {ok, Mounted};
        false -> {error, ["Could not load module from any of ", [Notmounted]]}
      end;
    {error, Error} -> {error, Error}
  end.

mount_file(Fspath, Type, Mounter) ->
  Suffix = hty_fspath:ext(Fspath) ++ "_" ++ Type,
  Prefixes = Mounter#hty_mounter.prefixes,
  Candidates = lists:map(fun(Prefix) -> 
    Prefix ++ "_" ++ Suffix 
  end, Prefixes),
  CandidateResults = lists:map(fun(Candidate) ->
    case code:ensure_loaded(list_to_atom(Candidate)) of
      {module, Module} ->
        case Module:mount(Fspath, Mounter) of 
          {ok, Instance} ->
            {ok, Instance};
          {error, Error} ->
            {error, Error}
        end;
      {error, nofile} ->
        {notfound, Candidate};
      {error, Error} ->
        {error, Error}
    end
  end, Candidates),
  lists:foldl(fun(CandidateResult, ResultSoFar) ->   
    case {CandidateResult, ResultSoFar} of         
      {{ok, _} = Ok, result0} -> Ok;
      {{ok, _} = Ok, {notfound, _}} -> Ok;
      {{ok, _}, {error, _} = Error} -> Error;
      {{ok, One}, {ok, Two}} -> {error, ["Multiple loadable candidates ", One, Two, "."]};

      {{notfound, _} = Notfound, result0} -> Notfound;
      {{notfound, Notfound}, {notfound, Notfounds}} -> {notfound, [Notfound|Notfounds]};
      {{notfound, _}, {ok, _} = Ok} -> Ok;
      {{notfound, _}, {error, _} = Error} -> Error;      
      
      {{error, Error}, {error, Errors}} -> {error, [Error|Errors]};
      {{error, _} = Error, _} -> Error
    end    
  end, result0, CandidateResults).
    

%mount(Fspath, Type, Mounter) ->
%  Suffix = hty_fspath:ext(Fspath) ++ "_" ++ Type,
%  Prefixes = Mounter#hty_mounter.prefixes,
%  case mount2(Prefixes, Suffix, []) of
%    {ok, Module} ->
%      Module:mount(Fspath, Mounter);
%    {error, Error, Fails} ->
%      Msg = "Could not load module from any of",
%      {error, {Msg, Fails, Error}}
%  end.

%mount2([Prefix|Prefixes], Suffix, Fails) ->
%  Name = Prefix ++ "_" ++ Suffix,
%  case code:ensure_loaded(list_to_atom(Name)) of
%    {module, Module} ->
%      {ok, Module};
%    {error, Error} ->
%      case Prefixes of
%        [] ->
%          {error, Error, [Name|Fails]};
%        _ ->
%          mount2(Prefixes, Suffix, [Name|Fails])
%      end
%  end.

walk(Fspath, Type, Mounter) ->
  mount_children_or_fail(Fspath, Type, Mounter).

%walk_old(Fspath, [T|_] = Type, Mounter) when is_integer(T) ->
%  walk_old(Fspath, [Type], Mounter);
%walk_old(Fspath, Types, Mounter) ->
%  Files = sort(hty_fspath:list(Fspath)),
%  io:format("Walking ~p~n", [Files]),
%  lists:foldl(fun(Item, Result) -> 
%      case {walk2(Files, Item, [], Mounter), Result} of 
%        {{ok, _},{ok, _}} -> {error, " duplicate mount matches for " ++ hty_fspath:path(Item)};
%        {{ok, _} = Ok, {error, _}} -> Ok;
%        {{error, _}, {ok, _} = Ok} -> Ok;
%        {{error, _} = Error, {error, _}} -> Error
%      end
%    end, {error, hty_mounter_walk_with_empty_list_of_types}, Types).

%walk2([], _, Mounts, _) -> {ok, lists:reverse(Mounts)};
%walk2([Fspath|Fspaths], Type, Mounts, Mounter) ->
%  case mount(Fspath, Type, Mounter) of
%    {ok, Mount} ->
%      walk2(Fspaths, Type, [Mount|Mounts], Mounter);
%    {error, _} = Error ->
%      Error
%  end.

sort(List) -> lists:sort(List).
