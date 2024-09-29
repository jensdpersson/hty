-module(hty_assets_resource).

-export([mount/2, handle/2]).

mount(Fspath, _Mc) ->

  case hty_fspath:type(Fspath) of
    dir -> 
        {hty_assets_resource, Fspath};
    file -> 
        case hty_fspath:load(Fspath) of
            {ok, Binary} ->
                Fspath1 = hty_fspath:new(Binary),
                {ok, {hty_assets_resource, Fspath1}};
            {no, Error} ->
                {no, Error}
        end
  end.

handle(Htx, {hty_assets_resource, Fspath}) -> 
    hty_fileserver:serve(Htx, Fspath).