%@doc file system utility.
-module(hty_fs).

-export([cursor/1]).

cursor(Path) -> 
	      Str = atom_to_list(Path),
	      Segments = filename:split(Str),
	      hty_fs_cursor:new(Segments, Path).



		  

	

     
