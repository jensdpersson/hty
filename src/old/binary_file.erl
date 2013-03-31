-module(binary_file, [Entity]).
-behaviour(resource).
-export([next/2, handle/2]).

next(_Evt,_) -> not_supported.
handle(_Evt,_) -> not_supported.
    
