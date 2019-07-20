%% Author: jens
-module(hty_putfile_resource).
-record(hty_putfile_resource, {key}).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([mount/1, handle/2, new/1]).


mount(Fspath) ->
  case lists:reverse(Fspath:parts()) of
    ["putfile", Key | _] ->
      {ok, new(Key)};
    _ ->
      {error, "putfile resource requires a storage key parameter"}
  end.

new(Key) ->
    #hty_putfile_resource{key=Key}.

handle(Htx, This) ->
  case Htx:method() of 
      'PUT' ->
	  do_put(Htx, This);
      _ ->
	  Htx:method_not_allowed(['PUT'])
  end.

do_put(Htx, This) ->
  Key = This#hty_putfile_resource.key,
  case hty_pathmapper:htx_to_fspath(Htx, Key) of
    {ok, Fspath} ->
      case Fspath:isdir() of
        true ->
          Htx:forbidden();
        false ->
	      Fsparent = Fspath:parent(), 
	      case Fsparent:exists() of
	      	   true ->
		      Htx1 = Fspath:recv([], Htx),
		      Htx2 = Htx1:ok(),
		      Htx2:commit();
		   false ->
		      Htx:not_found()
	      end
      end;
    _ ->
      erlang:display("Pathmapper not found")
  end.


