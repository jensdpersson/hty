-module(hty_request, [State]).











header(Name) -> lists:keyfind(State#http.headers, 1, Name).


