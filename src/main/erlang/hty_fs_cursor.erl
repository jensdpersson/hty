-module(hty_fs_cursor, [Segments, Path]).

-export([exists/0]).

exists() -> filelib:is_file(Path).
