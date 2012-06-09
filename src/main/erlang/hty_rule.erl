%@doc A mounting rule. Instances of this behaviour are presented with mountables that represent file system entries
%     and may mount resources accordingly.
-module(hty_rule).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{mount, 1}];
behaviour_info(_) -> undefined.
    
