-module(hty_figlet).
-export([
    watch/3,
    new_lister_linear/1,
    new_lister_seq/1,
    new_lister_file/1
]).

-export([start/2, stop/1]).

-type subtree_collector() :: fun((list(figtree())) -> ok|no). 
-type lister() :: fun((subtree_collector()) -> ok|no).
-type fig() :: string().
-type figtree() :: {fig(), lister()}.
-type figlet_resolver() :: fun((fig()) -> figlet()).
-type figlet() :: tuple(). 
-type figlet_collector() :: fun((figlet()) -> ok).

% Perhaps the figtree structure needs an etag or last-modified that can be used to
% only watch/3 the stale subtrees. 

-spec watch(fig(), figlet_resolver(), figlet_collector()) -> any().
watch({Root, Lister}, FigletResolver, FigletCollector) ->
    % This is the async part, and the part which can happen many times
    % if underlying structure is modified when running.
    Lister(fun(Subtrees) -> 
        % Zero or more subtrees to process
        % Watch each one in turn, folding the result over
        % the current focus/root node.
        ParentFiglet = FigletResolver(Root),
        apply_children(Subtrees, FigletResolver, ParentFiglet, fun(FoldedParent) ->
            FigletCollector(FoldedParent)
        end)
    end).

% For each subfigtree, descend into it, apply the result to parent and then descend into next
% When all children are applied report the folded result back in the FigletCollector callback.
apply_children([], _FigletResolver, Parent, FigletCollector) -> FigletCollector(Parent);
apply_children([Subfigtree|Subfigtrees], FigletResolver, Parent, FigletCollector) ->
    watch(Subfigtree, FigletResolver, fun(Subfiglet) ->
        Parent1 = apply_to_parent(Parent, Subfiglet),
        apply_children(Subfigtrees, FigletResolver, Parent1, FigletCollector)
    end).    

% Todo, the figlet module might not be able to apply itself to the parent, propagate error.
-spec apply_to_parent(Parent::figlet(), Child::figlet()) -> ParentWithChildApplied::figlet().
apply_to_parent(Parent, Child) ->
    Module = element(1, Child),
    Parent1 = Module:apply_to_parent(Parent, Child),
    Parent1.


% This lister will follow a linear sequence of single children
% and always construct from the next segment a single child fig
new_lister_linear(SegmentsOrEmpty) ->
    case SegmentsOrEmpty of
        [] -> fun lister_empty/1;
        [Segment|Segments] ->
            fun(SubtreeCollector) ->
                Fig = create_fig(Segment, new_lister_linear(Segments)),
                SubtreeCollector([
                    Fig
                ])
            end
    end.
    
lister_empty(SubtreeCollector) -> SubtreeCollector([]).

new_lister_seq(Listers) ->
    case Listers of
        [] -> fun lister_empty/1;
        _ -> 
            fun(SubtreeCollector) ->
                invoke_lister(Listers, SubtreeCollector)
            end
    end.
    
invoke_lister([], SubtreeCollector) -> SubtreeCollector([]);
invoke_lister([Lister|Listers], SubtreeCollector) ->
    Lister(fun(Result1) -> 
        case Result1 of 
            [] ->
                invoke_lister(Listers, SubtreeCollector);
            Children ->
                SubtreeCollector(Children)
        end
    end).
    
    
new_lister_file(Filesystempath) ->
    case file:list_dir(Filesystempath) of
        [] -> fun lister_empty/1;
        Filenames ->
            Figs = lists:map(
                fun(Filename) ->
                    create_fig(Filename, 
                        new_lister_file(filename:absname_join(Filesystempath, Filename))
                    )  
                end,
                Filenames
            ),
            fun(SubtreeCollector) ->
                SubtreeCollector(Figs)
            end
    end.
    
    
create_fig(Name, Lister) -> {Name, Lister}.

% De här funktionerna är mer hty-specifika. Kanske de får hamna i en egen hty_figlet
% och det generiska blir bara figlet.
-spec start(string(), string()) -> ok|{no, string()}.
start(Prepath, Rootfolder) ->
    
    Pid = spawn(fun loop_supervise/1),
    
    Root = hty_root:new(),
    Lister = new_lister_seq(
        [new_lister_linear(Prepath), new_lister_file(Rootfolder)]
    ),
    
    Resolver = fun(Name) ->
        % Få se, en figlet behöver ju vara en tuple till att börja med.
        % Och första elementet ska vara en modul.
        % Om vi bara mappar rakt av 
        % hty_server_http.8080 -> {hty_server_http, 8080}
        % nu-hty har
        % 8080.http -> {hty_server_http, 8080}
        % 1.hty_server_http.8080
        % Första segmentet behöver kunna sorteras på.
        % Sista extension kan vara praktiskt att låta heta xml eller ini
        % om resursen har sitt data så
        % Man kan kanske ska ha en konvention för att hitta modulen och sen delegera.
        % Första elementet funkar dåligt med sortering. Sista-fast-inte-om-det-står-xml känns
        % stökigt. Jag tror man måste köra sista.
        % Alltså
        case string:split(Name, $.) of
            [] -> {no, {empty_filename}};
            Segments -> 
                [Module|Rest] = lists:reverse(Segments),
                Module:new(Rest)     
        end
    end,
    
    watch({Root, Lister}, Resolver, fun(Figlet) -> 
        Pid ! {refresh, Figlet}
    end),
    
    {ok, Pid}.
    
stop(Pid) ->
    Pid ! {stop, self()}.
    
loop_supervise(Servers) ->
    receive 
        {refresh, _Figlet} -> 
            % TODO reconfigure
            loop_supervise(Servers);
        {stop, ReplyTo} ->
            ReplyTo ! stopping
    end.        
