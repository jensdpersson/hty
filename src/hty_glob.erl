-module(hty_glob).


-export([find/2, parse/1, glob/2]).

-type glob() :: string().
-export_type([glob/0]).


% A segment can contain a * as a wildcard
% or be just a filename
% TODO "**" multifolder glob
-spec find(Segs::list(), Fspath::any()) -> [string()].
find(Segs, Path) ->
    find_internal(Segs, Path, []).
    
% Segs are path segments,
% Path is the context fs location
% Above is the path prefix to include
find_internal([], _, Above) -> [lists:reverse(Above)];
find_internal([Seg|Segs], Path, Above) ->
    % Check for globs
    case parse(Seg) of
        % No globs, go straight to subfile
        no_globs ->
        
            % Check if such a file exists.
            Subpath = hty_fspath:subpath([Seg], Path),
            case hty_fspath:exists(Subpath) of
                true -> find_internal(Segs, Subpath, [Seg|Above]);
                false -> []
            end;
            
        double_glob -> 
            case hty_fspath:isdir(Path) of
                true ->
                    case hty_fspath:list(Path) of 
                        {error, _} = Error ->
                            Error;
                        Subpaths ->
                            io:format("** Subpaths ~p ~n", [Subpaths]),
                            lists:flatmap(fun(Subpath) ->
                                Matches = find_internal(Segs, Subpath, [hty_fspath:basename(Subpath)|Above]),
                                Globs = find_internal([Seg|Segs], Subpath, [hty_fspath:basename(Subpath)|Above]),
                                case {Matches, Globs} of
                                    {[],[]} -> [];
                                    {[], _} -> Globs;
                                    {_, []} -> Matches;
                                    _ -> Matches ++ Globs
                                end
                            end, Subpaths)
                    end;
                false -> []
            end;
            
        % Yes, there are globs, list subs and filter
        Filter ->
            case hty_fspath:isdir(Path) of
                true ->
                    case hty_fspath:list(Filter, Path) of 
                        {error, _} = Error ->
                            Error;
                        Subpaths ->
                            io:format("Subpaths ~p ~n", [Subpaths]),
                            lists:flatmap(fun(Subpath) ->
                                find_internal(Segs, Subpath, [hty_fspath:basename(Subpath)|Above])
                            end, Subpaths)
                    end;
                false -> []
            end
    end.
    


-spec parse(Token::string()) -> no_globs | glob().
parse("**") -> double_glob;
parse(Token) -> 
    case string:find(Token, [$*]) of
        nomatch -> no_globs;
        _ -> fun(Fspath) -> glob(Token, hty_fspath:basename(Fspath)) end
    end.
    
%%%%% match %%%%%%

-spec glob(Matcher::string(), Matchee::string()) -> boolean().    
    
% Final wildcard
glob([$*], _) -> true;

% Non-final wildcard 
glob([$*, X|RestOfMatcher] = Matcher, [X|RestOfInput]) ->
    case glob(RestOfMatcher, RestOfInput) of
        % ok, this one matched
        true -> true;
        
        % try the wild some more
        false -> glob(Matcher, RestOfInput)
    end;

% Initial wildcard non-match
glob([$*|_] = Matcher, [_|RestOfInput]) -> glob(Matcher, RestOfInput);

% Non-wildcard match    
glob([X|RestOfMatcher], [X|RestOfInput]) -> glob(RestOfMatcher, RestOfInput);

% Non-match    
glob([_|_], [_|_]) -> false;

% Full match
glob([], []) -> true;

% Input too short
glob(_, []) -> false;

% non-comprehensive matcher
glob([], _) -> false.

%%%%% /match %%%%%%

