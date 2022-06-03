-module(hty_neatpath).
-record(hty_neatpath, {name, preds}).
-export([with_name/1, add_param/2]).
-type pred() :: {Functor :: string(), [Param :: string()]}.


