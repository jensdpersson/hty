-module(hty_figlet_SUITE).

-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([single_level/1]).

all() -> [
    {group, foldered}
].

groups() ->
    [{foldered, [
        single_level
    ]}].

init_per_group(Group, Config) ->
    Groupname = atom_to_list(Group),
    Groupdir = proplists:get_value(data_dir, Config) ++ Groupname,
    [{group_dir, Groupdir}|Config].

end_per_group(_Group, Config) -> Config.
    
init_per_testcase(Test, Config) -> 
    Fixture = proplists:get_value(group_dir, Config) ++ "/" ++ atom_to_list(Test),
    [{fixture, Fixture}|Config].

end_per_testcase(_Test, _Config) -> ok.

-record(testcase, {prefix, facit}).

run_test(Cfg, Testcase) ->
    Rootdir = proplists:get_value(fixture, Cfg),
    Self = self(),
    
    Figtree = {
        "root.hty_figlet_test",
        hty_figlet:new_lister_file(Rootdir)
    },
    
    Resolver = hty_figlet:new_resolver_modfromext(),
    
    Collector = fun(Figlet) ->  
        Figlet = Testcase#testcase.facit,
        Self ! {ok, Figlet}
    end,
    
    {ok, Pid} = hty_figlet:watch(Figtree, Resolver, Collector),
    receive 
        {ok, result} -> 
            ok
        after 
            1000 -> 
            Pid ! stop,
            fail
    end.

single_level(Cfg) -> 
    run_test(Cfg, #testcase{
        prefix=[],
        facit={hty_figlet_test, "ett", "tu", "tre"}
    }).

