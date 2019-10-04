-module(hty_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(hty_sup, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => hty_main,
                    start => {hty_main, start, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [hty_sup]}],
    {ok, {SupFlags, ChildSpecs}}.
